{-# LANGUAGE LambdaCase #-}
module Eval where
import Prelude hiding (pred, succ, and, or)
import Core
import Syntax
import Terms
import Env
import Data.Functor.Identity

eval :: Monad m => Expr -> EnvT Action m Action
eval (VarExp n) = findM n >>= \case 
  (Just v) -> return v
  (Nothing) -> return $ Param n
eval (TermExp t) = return $ TermAct t
eval (BoolExp b) = return $ BoolAct b
eval (NumExp n) = return $ NumAct n
eval (CharExp c) = return $ CharAct c
eval (StringExp s) = return $ StringAct s
eval (ListExp xs) = do
  es <- sequence $ map eval xs
  return $ ListAct es
eval m@(Match x (y,e1) (s,t,e2) (z,u,e3)) = eval x >>= \case 
  (TermAct (Var (Name y'))) -> (defineM y (StringAct y') ) >> eval e1
  (TermAct (App s' t')) -> defsM [(s, TermAct s'),(t, TermAct t')] >> eval e2
  (TermAct (Abs (Name z') u')) -> defsM [( u,TermAct u'), (z,StringAct z')] >> eval e3
  v -> envM >>= \env -> return $ Application v [Closure [y] e1 env, Closure [s,t] e2 env, Closure [z,u] e3 env]
eval (If c e1 e2) = eval c >>= \case 
  BoolAct b -> if b then eval e1 else eval e2 
  v -> eval e1 >>= \a1 -> eval e2 >>= \a2 -> return $ Application v [a1,a2]
eval (Func xs e) = envM >>= \env -> return $ Closure xs e env
eval (Apply e es) = do
  a <- eval e
  as <- sequence $ map eval es
  applyM a as
eval (Let d e) = elabM d >> eval e 

elabM :: Monad m => Defn -> EnvT Action m () 
elabM (Val x e) = eval e >>= defineM x
elabM (Rec x e) = envM >>= \env -> defineM x (DefRec x e env)

elabsM :: Monad m => [Defn] -> EnvT Action m ()
elabsM = sequence_ . map elabM

act :: Expr -> Env -> Action
act = runId eval 

runId :: (a -> EnvT b Identity c) -> a -> Environment b -> c
runId f a =  Prelude.fst . runIdentity . runEnvT (f a)

applyM :: Monad m => Action -> [Action] -> EnvT Action m Action
applyM (Closure xs e env) as = inM env >> defsM (zip xs as) >> eval e 
applyM (Primitive p) as = return $ applyPrim p as 
applyM a as = return $ Application a as

-- Currently allows things like (+ 'a' 'b'), should this result in an error?
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

evalP :: Monad m => Program -> EnvT Action m Action
evalP (Program ds e) = elabsM ds >> eval e

-- this elaborates the environment sequentially
act' :: Program -> Env -> Action
act' = runId evalP 

partial :: Action -> Partial Combinator 
partial (TermAct t) = Hole (CTerm t)
partial (NumAct n) = Hole (CInt n)
partial (BoolAct a) = Hole (CBool a)
partial (CharAct c) = Hole (CChar c)
partial (StringAct s) = Hole (CString s)
partial (ListAct xs) = Hole (CList (map partial xs))
partial (Param x) = PVar x
partial (Closure (xs) e env) = let y:ys = reverse xs in 
  foldr (PAbs) (PAbs y (partial $ runId eval e env)) (reverse ys) 
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
 
prims :: Env
prims = map (\(n,p) -> (Name n, Primitive p)) 
  [ ("+", Plus)
  , ("-", Minus)
  , ("*", Times)
  , ("/", Div)
  , ("%", Mod)
  , ("and", And)
  , ("or", Or)
  , ("not", Not)
  , ("==", Equal)
  , ("<", Lesser)
  , ("<=", Leq)
  , (">=", Geq)
  , (">", Greater)
  , ("=c", ChEqual)
  , ("head", Head)
  , ("tail", Tail)
  , ("cons", Cons)
  , ("empty", Empty)
  , ("=s", StrEqual) 
  , ("Var", VAR) 
  , ("App", APP) 
  , ("Abs", ABS) ]

consts = map (\(n,a) -> (Name n, a)) 
  [("nil", ListAct []) ]

prim = prims ++ consts
