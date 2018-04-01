module LamE where
import Prelude hiding (pred, succ, and, or)
import Core
import Syntax
import Terms

--Env -> Action
act :: Expr -> Env -> Action
act (VarExp n) env = case find env n of
  Just v -> v
  Nothing -> Param n
act (TermExp t) env = TermAct t
act (BoolExp b) env = BoolAct b
act (NumExp n) env = NumAct n
act (CharExp c) env = CharAct c
act (StringExp s) env = StringAct s
act (ListExp xs) env = ListAct (map (\e -> act e env) xs)
act m@(Match x (y,e1) (s,t,e2) (z,u,e3)) env = case act x env of --hmmmm
  (TermAct (Var (Name y'))) -> act e1 (define env y (StringAct y') )
  (TermAct (App s' t')) -> act e2 (define (define env s (TermAct s')) t (TermAct t') )
  (TermAct (Abs (Name z') u')) -> act e3 (define (define env u (TermAct u')) z (StringAct z'))
  v -> Application v [Closure [y] e1 env, Closure [s,t] e2 env, Closure [z,u] e3 env]
   {- where 
    env1 = (define env y (StringAct y') )
    env2 = (define (define env s (TermAct s')) t (TermAct t') )
    env3 = (define (define env u (TermAct u')) z (StringAct z')) -}
act (If c e1 e2) env = case (act c env) of
  BoolAct b -> if b then (act e1 env) else (act e2 env)
  v -> Application v [act e1 env, act e2 env]
act (Func xs e) env = Closure xs e env
act (Apply e es) env = apply (act e env) (map (\e -> act e env) es)
act (Let d e) env = act e (elab d env)

apply :: Action -> [Action] -> Action
apply (Closure xs e env) as = act e (defargs env xs as)
apply (Primitive p) as = applyPrim p as 
apply a as = Application a as

--not ideal, + 'a' 'b' _should_ result in an error
applyPrim :: Prim -> [Action] -> Action
applyPrim (Plus) [NumAct n, NumAct m] = NumAct (n+m)
applyPrim (Minus) [NumAct n, NumAct m] = NumAct (n-m)
applyPrim (Times) [NumAct n, NumAct m] = NumAct (n*m)
applyPrim (Div) [NumAct n, NumAct m] = NumAct (n `div` m)
applyPrim (Mod) [NumAct n, NumAct m] = NumAct (n `mod` m)
applyPrim (And) [BoolAct n, BoolAct m] = BoolAct (n&&m)
applyPrim (Or) [BoolAct n, BoolAct m] = BoolAct (n||m)
applyPrim (Not) [BoolAct n] = BoolAct (not n)
applyPrim (Equal) [NumAct n, NumAct m] = BoolAct (n==m)
applyPrim (Lesser) [NumAct n, NumAct m] = BoolAct (n<m)
applyPrim (Leq) [NumAct n, NumAct m] = BoolAct (n<=m)
applyPrim (Geq) [NumAct n, NumAct m] = BoolAct (n>=m)
applyPrim (Greater) [NumAct n, NumAct m] = BoolAct (n>m)
applyPrim (ChEqual) [CharAct n, CharAct m] = BoolAct (n==m)
applyPrim (Head) [ListAct xs] = (head xs) 
applyPrim (Tail) [ListAct xs] = ListAct (tail xs) 
applyPrim (Cons) [x, ListAct xs] = ListAct (x:xs) 
applyPrim (Empty) [ListAct xs] = BoolAct (xs==[]) 
applyPrim (StrEqual) [StringAct n, StringAct m] = BoolAct (n==m)
applyPrim (VAR) [StringAct n] = TermAct (Var (Name n)) 
applyPrim (APP) [TermAct s, TermAct t] = TermAct (App s t) 
applyPrim (ABS) [StringAct n, TermAct s] = TermAct (Abs (Name n) s) 
applyPrim p xs = Application (Primitive p) xs

defargs :: Env -> [Name] -> [Action] -> Env
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (act e env)
elab (Rec x e) env = define env x (DefRec x e env)

-- this elaborates the environment sequentially
act' :: Program -> Env -> Action
act' (Program ds e) env = act e (foldr elab env (reverse ds))

--Action -> Term, in several steps
partial :: Action -> Partial Combinator 
partial (TermAct t) = Hole (CTerm t)
partial (NumAct n) = Hole (CInt n)
partial (BoolAct a) = Hole (CBool a)
partial (CharAct c) = Hole (CChar c)
partial (StringAct s) = Hole (CString s)
partial (ListAct xs) = Hole (CList (map partial xs))
partial (Param x) = PVar x
partial (Closure (xs) e env) = let y:ys = reverse xs in foldr (PAbs) (PAbs y (partial $ act e env)) (reverse ys) 
partial (DefRec x e env) = PApp (Hole Y) (partial $ Closure [x] e env)
partial (Application f (e:es)) = foldl (PApp) (PApp (partial f) (partial e)) $ map partial es
partial (Primitive p) = Hole (CPrim p)

instance Church Prim where
  church p = case p of
    Plus -> addInt
    Minus -> minusInt 
    Times -> timesInt
    Div -> divideInt 
    Mod -> modInt
    And -> and
    Or -> or
    Not -> neg
    Equal -> equalInt
    Lesser -> lesserInt
    Leq -> leqInt
    Geq -> geqInt
    Greater -> greaterInt
    ChEqual -> equalChar
    Head -> headT
    Tail -> tailT
    Cons -> cons
    Empty -> emptyList
    StrEqual -> equalString
    VAR -> varT
    APP -> appT
    ABS -> absT   

instance Church a => Church (Partial a) where
  church p = case p of
    (PVar x) -> Var x
    (PApp s t) -> App (church s) (church t)
    (PAbs x s) -> Abs x (church s)
    (Hole x) -> church x 

instance Church Combinator where
  church x = case x of
    (CPrim p) -> church p
    (CInt n) -> church n 
    (CBool b) -> church b
    (CChar c) -> church c
    (CList xs) -> church $ map church xs
    (CString s) -> church s
    (CTerm t) -> church t
    (Y) -> y 
