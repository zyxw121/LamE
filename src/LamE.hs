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

applyPrim :: Prim -> [Action] -> Action
applyPrim (Plus) [NumAct n, NumAct m] = NumAct (n+m)
applyPrim (Plus) [x,y] = Application (Primitive Plus) [x,y]
applyPrim (Minus) [NumAct n, NumAct m] = NumAct (n-m)
applyPrim (Minus) [x,y] = Application (Primitive Minus) [x,y]
applyPrim (Times) [NumAct n, NumAct m] = NumAct (n*m)
applyPrim (Times) [x,y] = Application (Primitive Times) [x,y]
applyPrim (Div) [NumAct n, NumAct m] = NumAct (n `div` m)
applyPrim (Div) [x,y] = Application (Primitive Div) [x,y]
applyPrim (Mod) [NumAct n, NumAct m] = NumAct (n `mod` m)
applyPrim (Mod) [x,y] = Application (Primitive Mod) [x,y]
applyPrim (And) [BoolAct n, BoolAct m] = BoolAct (n&&m)
applyPrim (And) [x,y] = Application (Primitive And) [x,y]
applyPrim (Or) [BoolAct n, BoolAct m] = BoolAct (n||m)
applyPrim (Or) [x,y] = Application (Primitive Or) [x,y]
applyPrim (Not) [BoolAct n] = BoolAct (not n)
applyPrim (Not) [x] = Application (Primitive Not) [x]
applyPrim (Equal) [NumAct n, NumAct m] = BoolAct (n==m)
applyPrim (Equal) [x,y] = Application (Primitive Equal) [x,y]
applyPrim (Lesser) [NumAct n, NumAct m] = BoolAct (n<m)
applyPrim (Lesser) [x,y] = Application (Primitive Lesser) [x,y]
applyPrim (Leq) [NumAct n, NumAct m] = BoolAct (n<=m)
applyPrim (Leq) [x,y] = Application (Primitive Leq) [x,y]
applyPrim (Geq) [NumAct n, NumAct m] = BoolAct (n>=m)
applyPrim (Geq) [x,y] = Application (Primitive Geq) [x,y]
applyPrim (Greater) [NumAct n, NumAct m] = BoolAct (n>m)
applyPrim (Greater) [x,y] = Application (Primitive Greater) [x,y]
applyPrim (ChEqual) [CharAct n, CharAct m] = BoolAct (n==m)
applyPrim (ChEqual) [x,y] = Application (Primitive ChEqual) [x,y]
applyPrim (Head) [ListAct xs] = (head xs) 
applyPrim (Head) [x] = Application (Primitive Head) [x]
applyPrim (Tail) [ListAct xs] = ListAct (tail xs) 
applyPrim (Tail) [x] = Application (Primitive Tail) [x]
applyPrim (Cons) [x, ListAct xs] = ListAct (x:xs) 
applyPrim (Cons) [x, y] = Application (Primitive Cons) [x,y]
applyPrim (Empty) [ListAct xs] = BoolAct (xs==[]) 
applyPrim (Empty) [x] = Application (Primitive Empty) [x]
applyPrim (StrEqual) [StringAct n, StringAct m] = BoolAct (n==m)
applyPrim (StrEqual) [x,y] = Application (Primitive StrEqual) [x,y]
applyPrim (VAR) [StringAct n] = TermAct (Var (Name n)) 
applyPrim (VAR) [x] = Application (Primitive VAR) [x]
applyPrim (APP) [TermAct s, TermAct t] = TermAct (App s t) 
applyPrim (APP) [x,y] = Application (Primitive APP) [x,y]
applyPrim (ABS) [StringAct n, TermAct s] = TermAct (Abs (Name n) s) 
applyPrim (ABS) [x,y] = Application (Primitive ABS) [x,y]
applyPrim p xs = Application p xs


defargs :: Env -> [Name] -> [Action] -> Env
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (act e env)
elab (Rec x e) env = define env x (DefRec x e env)

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

term' :: (a -> Term) -> Partial a -> Term
term' f (PVar x) = Var x
term' f (PApp s t) = App (term' f s) (term' f t)
term' f (PAbs x s) = Abs x (term' f s)
term' f (Hole x) = f x

termP :: Prim -> Term
termP Plus = addInt
termP Minus = minusInt 
termP Times = timesInt
termP Div = divideInt 
termP Mod = modInt
termP And = and
termP Or = or
termP Not = neg
termP Equal = equalInt
termP Lesser = lesserInt
termP Leq = leqInt
termP Geq = geqInt
termP Greater = greaterInt
termP ChEqual = equalChar
termP Head = headT
termP Tail = tailT
termP Cons = cons
termP Empty = emptyList
termP StrEqual = equalString
termP VAR = varT
termP APP = appT
termP ABS = absT



termC :: Combinator -> Term
termC (CPrim p) = termP p
termC (CInt n) = churchInt n 
termC (CBool True) = true
termC (CBool False) = false
termC (CChar c) = churchChar c
termC (CList xs) = churchList $ map term xs
termC (CString s) = churchString s
termC (CTerm t) = churchTerm t
termC (Y) = y 

term :: (Partial Combinator) -> Term
term = term' termC
