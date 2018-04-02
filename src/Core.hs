{-# LANGUAGE  RecordWildCards #-}
module Core where
import Control.Applicative
import Prelude hiding (until)
import Data.List (unfoldr, intercalate)

newtype Name = Name String deriving (Eq)
data Term = Var Name | Abs Name Term | App Term Term deriving (Eq)
data DBTerm = DBVar Int | DBAbs DBTerm | DBApp DBTerm DBTerm deriving (Eq)

-- Of course, the Eq instance for terms is only syntactic, not equality in
-- lambda-beta (worse than syntactic in fact, it ignores alpha conversion)

instance Show Name where
  show (Name s) = s

-- gen n returns all names consisting of at most n lowercase letters
gen :: Int -> [Name]
gen 0 =[Name "a"]
gen 1 = map (Name . (:[])) ['a'..'z']
gen n = let ns = gen (n-1) in [Name (s:ss) | s <- ['a'..'z'], (Name ss) <- ns  ]

fresh' :: [Name] -> Name
fresh' ns =
  let n = length ns
      k = length $ takeWhile (<=n) $ iterate (*26) 1 
      ps = take (n+1) $ gen k  in 
        head [q | q<- ps, not $ q `elem` ns] 

-- It seems like returning a Set, or at least an ordered list (and merging in
-- the App case) would be more quicker, but it's actually noticably slower than
-- lists for reasonable inputs
frees :: Term -> [Name]
frees (Var n) = [n]
frees (App s t) = frees s ++ frees t
frees (Abs x s) = filter (/=x) $ frees s        

fresh :: [(a,Name)] -> Name
fresh es = let (as,names) = unzip es in fresh' names

--Conversions between term types
index :: (Eq a) => [a] -> a -> Int
index [] a = error "no index"
index (x:xs) a = if x==a then 0 else 1+(index xs a)

toDB' :: Term -> [Name] -> DBTerm
toDB' (Var n) ns = DBVar $ index ns n 
toDB' (App s t) ns = DBApp (toDB' s ns) (toDB' t ns)
toDB' (Abs x s) ns = DBAbs (toDB' s (x:ns))

toDB :: Term -> DBTerm
toDB t = toDB' t (frees t) 

find' :: (Eq a) => [(a,Name)] -> a -> Name
find' es n = let matches = filter (\(x,s) -> x==n) es in case matches of
  [] -> fresh es 
  ((x,s):xs) -> s

fromDB' :: [(Int, Name)] -> DBTerm -> Term
fromDB' es (DBVar n) = Var (find' es n)
fromDB' es (DBApp s t) = App (fromDB' es s) (fromDB' es t)
fromDB' es (DBAbs s) = let x = fresh es in Abs x (fromDB' ((0,x):(map (\(a,b) -> (a+1,b)) es)) s)

fromDB :: DBTerm -> Term
fromDB t = fromDB' [] t

--Pretty printing
addParens :: Int -> String -> String
addParens 0 s = s
addParens n s = "(" ++ s ++ ")"

vars :: Term -> [Name]
vars (Abs n s) = n:(vars s)
vars _ = []

body :: Term -> Term
body (Abs n s) = body s
body t = t

pp :: Int -> Term -> String
pp r (Var n) = show n
pp r (App s t) = addParens r $ pp (r+1) s ++ " " ++  pp r t
pp r t@(Abs n s) = let (vs, b) = (map show $ vars t, body t) in addParens r $ "\\" ++ (intercalate " " vs) ++"." ++ pp (r+1) b  

instance Show Term where
  show = pp 0

instance Show DBTerm where
  show = pp 0 . fromDB

-- Substitution
sub (Var y) t x  = if x==y then t else (Var y)
sub (App u v) t x  = App (sub u t x) (sub v t x)
sub (Abs y s) t x = let ts = frees t in
  if y==x || y `elem` ts then let z = fresh' (ts ++ frees s ++ [x]) in Abs z $ sub (sub s (Var z) y) t x 
  else Abs y (sub s t x)

--shift' n m t increments all vars in t that are > than m by n
shift' :: Int -> Int -> DBTerm -> DBTerm
shift' n m (DBVar k) = if k >= m then (DBVar $ n+k) else (DBVar k)
shift' n m (DBApp u v) = DBApp (shift' n m u) (shift' n m v)
shift' n m (DBAbs s) = DBAbs (shift' n (m+1) s)

shift :: Int -> DBTerm -> DBTerm
shift n = shift' n 0

sub' :: DBTerm -> Int -> DBTerm -> DBTerm
sub' t n (DBVar m) = if n==m then t else (DBVar m)
sub' t n (DBApp u v) = DBApp (sub' t n u) (sub' t n v)
sub' t n (DBAbs s) = DBAbs (sub' (shift 1 t) (n+1) s ) 

-- Beta reduction

bred :: Term -> Maybe Term
bred (App (Abs x s) v) =  Just $ sub s v x
bred _ = Nothing

bred' :: DBTerm -> Maybe DBTerm
bred' (DBApp (DBAbs s) t) = Just $ shift (-1) (sub' (shift 1 t) 0 s ) 
bred' _ = Nothing

-- Leftmost reduction

lred :: Term -> Maybe Term
lred (Var x) = Nothing
lred u@(App s t) = bred u
  <|> do
    x <- lred s 
    return $ App x t
  <|> do
    x <- lred t
    return $ App s x 
lred (Abs x s) = lred s >>= Just . Abs x

lred' :: DBTerm -> Maybe DBTerm
lred' (DBVar x) = Nothing
lred' u@(DBApp s t) = bred' u 
  <|> do
    x <- lred' s
    return $ DBApp x t
  <|> do 
    x <- lred' t 
    return $ DBApp s x
lred' (DBAbs s) = lred' s >>= return . DBAbs

-- Beta normal form
until :: (a -> Maybe a) -> a -> a
until f x = case f x of
  Nothing -> x
  Just x' -> until f x'

bnf' :: DBTerm -> DBTerm
bnf' = until lred'

-- Very slow
bnf1 :: Term -> Term
bnf1 = until lred

-- Use this instead
bnf = fromDB . bnf' . toDB

-- Head reduction

unabs :: Term -> [Name] -> Maybe (Term, [Name])
unabs t@(App p q) ns = Just (t, ns)
unabs (Abs n s) ns =  unabs s (n:ns) 
unabs _ ns = Nothing

unapp :: Term -> [Term] -> Maybe (Name, Term, Term, [Term])
unapp (App (Abs x t) u) ts = Just (x, t, u, ts) 
unapp (App p q) ts = unapp p (q:ts) 
unapp _ ts = Nothing

hred :: Term -> Maybe Term
hred p = do
  (app, is) <- unabs p []
  (x,t,u,ts) <- unapp app []
  let h = sub t u x
      h' = foldl App h ts
      h'' = foldr Abs h' is
  return h''

-- Head normal form
hnf :: Term -> Term
hnf = until hred
