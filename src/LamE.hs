module LamE where
import Prelude hiding (pred, succ, and, or)
import Core
import Syntax


--Env -> Action
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
apply (Primitive p) as = applyP p as 
apply a as = Application a as

applyP :: Prim -> [Action] -> Action
applyP = undefined

defargs :: Env -> [Name] -> [Action] -> Env
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (act e env)
elab (Rec x e) env = define env x (DefRec x e env)

act' :: Program -> Env -> Action
act' (Program ds e) env = act e (foldr elab env  ds)

--Action -> Term, in several steps

partial :: Action -> Partial Combinator 
partial (NumAct n) = Hole (CInt n)
partial (BoolAct a) = Hole (CBool a)
partial (Param x) = PVar x
partial (Closure (x:xs) e env) = foldr (PAbs) (PAbs x (partial $ act e env)) xs
partial (DefRec x e env) = PApp (Hole Y) (partial $ Closure [x] e env)
partial (Application f (e:es)) = foldl (PApp) (PApp (partial f) (partial e)) $ map partial es
partial (Primitive p) = Hole (CPrim p)

term' :: (a -> Term) -> Partial a -> Term
term' f (PVar x) = Var x
term' f (PApp s t) = App (term' f s) (term' f t)
term' f (PAbs x s) = Abs x (term' f s)
term' f (Hole x) = f x

termP :: Prim -> Term
termP Plus = addInt
termP Minus = minusInt 
termP Times = timesInt
--termP Divide = divideInt
--termP Pred = predInt
--termP Succ = succInt
termP And = and
termP Or = or
termP Not = neg
termP Equal = equalInt
termP Lesser = lesserInt
termP Leq = leqInt
termP Geq = geqInt
termP Greater = greaterInt

termC :: Combinator -> Term
termC (CPrim p) = termP p
termC (CInt n) = churchInt n 
termC (CBool True) = true
termC (CBool False) = false
termC (Y) = y

term :: (Partial Combinator) -> Term
term = term' termC
