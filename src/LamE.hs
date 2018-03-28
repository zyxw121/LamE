module LamE where
import Core
import Syntax

act :: Expr -> Env -> Action
act (VarExp n) env = case find env n of
  Just v -> v
  Nothing -> Param n
act (BoolExp b) env = BoolAct b
act (NumExp n) env = NumAct n
act (If c e1 e2) env = case (act c env) of
  BoolAct b -> if b then (act e1 env) else (act e2 env)
  v -> Application v [act e1 env, act e2 env]
act (Func xs e) env = Closure xs e env
act (Apply e es) env = apply (act e env) (map (\e -> act e env) es)
act (Let d e) env = act e (elab d env)

apply :: Action -> [Action] -> Action
apply (Closure xs e env) as = act e (defargs env xs as)
apply (Primitive p) as = undefined -- fix this
apply a as = Application a as

defargs :: Env -> [Name] -> [Action] -> Env
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (act e env)
elab (Rec x e) env = define env x (DefRec x e env)




