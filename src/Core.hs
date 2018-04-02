{-# LANGUAGE  RecordWildCards #-}
module Core where
import Control.Applicative
import Prelude hiding (and, or, abs, pred, succ, fst, snd)
import Data.List (unfoldr)

--data Name = Name {name :: Char, num :: Int }  deriving (Eq)
data Term = Var Name | Abs Name Term | App Term Term deriving (Eq)

data DBTerm = DBVar Int | DBAbs DBTerm | DBApp DBTerm DBTerm deriving (Eq)

-- 0 <-> a_0, 1 <-> b_0, ... etc
--instance Enum Name where 
--  fromEnum (Name{..}) = ((fromEnum (name::Char))-96) + 26*num
--  toEnum n = Name {
--    name= toEnum (((n-1)`mod`26) +97), 
--    num=n `div` 26}

newtype Name = Name String deriving (Eq)

instance Show Name where
  show (Name s) = s

alloweds = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-+*/<>=%"

--prims = words "== + - * / < > <= >= pred succ and or not"

allowed' c = c `elem` alloweds

allowed :: String -> Bool
allowed [] = False
allowed s = all allowed' s 

pos :: Char -> Int
pos c = index' c alloweds where
  index' _ [] = error "no index\'"
  index' a (x:xs) = if x==a then 1 else 1 + (index' a xs) 

makes :: Int -> [Name]
makes 0 =[Name "a"]
makes 1 = map (Name . (:[])) alloweds
makes n = let ns = makes (n-1) in [Name (s:ss) | s <- alloweds, (Name ss) <- ns  ]

news :: [Name] -> Name
news ns =
  let n = length ns
      k = length $ takeWhile (<=n) $ iterate (*70) 1 
      ps = take (n+1) $ makes k  in 
        head [q | q<- ps, not $ q `elem` ns] 
          


--Conversions between term types
frees :: Term -> [Name]
frees (Var n) = [n]
frees (App s t) = frees s ++ frees t
frees (Abs x s) = filter (/=x) $ frees s

index :: (Eq a) => [a] -> a -> Int
index [] a = error "no index"
index (x:xs) a = if x==a then 0 else 1+(index xs a)

toDB' :: Term -> [Name] -> DBTerm
toDB' (Var n) ns = DBVar $ index ns n 
toDB' (App s t) ns = DBApp (toDB' s ns) (toDB' t ns)
toDB' (Abs x s) ns = DBAbs (toDB' s (x:ns))

toDB :: Term -> DBTerm
toDB t = toDB' t (frees t) 

--smallest :: [Int] -> Int
--smallest xs = foldr min (length xs+1) [x| x<- [1..length xs +1], not $x `elem` xs ]

fresh :: [(a,Name)] -> Name
fresh es = let (as,names) = unzip es in news names

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
--instance Show Name where
--  show (Name{..}) = if num>0 then [name]++"_"++(show num) else [name] 

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
pp r (App s t) = addParens r $ pp (r+1) s ++ pp r t
pp r t@(Abs n s) = let (vs, b) = (map show $ vars t, body t) in addParens r $ "\\" ++ (concat vs) ++"." ++ pp (r+1) b  

instance Show Term where
  show = pp 0

instance Show DBTerm where
  show = pp 0 . fromDB


-- Reduction



sub (Var y) t x  = if x==y then t else (Var y)
sub (App u v) t x  = App (sub u t x) (sub v t x)
sub (Abs y s) t x = let ts = frees t in
  if y==x || y `elem` ts then let z = news (ts ++ frees s ++ [x]) in Abs z $ sub (sub s (Var z) y) t x 
  else Abs y (sub s t x)

bred :: Term -> Maybe Term
bred (App (Abs x s) v) =  Just $ sub s v x
bred _ = Nothing

--be careful with this one
lred :: Term -> Maybe Term
lred (Var x) = Nothing
lred u@(App s t) = bred u <|> (lred s>>= (\x -> Just $ App x t))  <|> (lred t >>= Just . App s)
lred (Abs x s) = lred s >>= Just . Abs x

--This is still buggy
bnf :: Term -> Term
bnf t = case lred t of
  Nothing -> t
  Just t' -> bnf t'

--bnf = fromDB . bnf' . toDB

--shift' n m t increments all vars in t that are > than m by n
shift' :: Int -> Int -> DBTerm -> DBTerm
shift' n m (DBVar k) = if k >= m then (DBVar $ n+k) else (DBVar k)
shift' n m (DBApp u v) = DBApp (shift' n m u) (shift' n m v)
shift' n m (DBAbs s) = DBAbs (shift' n (m+1) s)

--shift n incs free vars by n
shift :: Int -> DBTerm -> DBTerm
shift n = shift' n 0

sub' :: DBTerm -> Int -> DBTerm -> DBTerm
sub' t n (DBVar m) = if n==m then t else (DBVar m)
sub' t n (DBApp u v) = DBApp (sub' t n u) (sub' t n v)
sub' t n (DBAbs s) = DBAbs (sub' (shift 1 t) (n+1) s ) 

bred' :: DBTerm -> Maybe DBTerm
bred' (DBApp (DBAbs s) t) = Just $ shift (-1) (sub' (shift 1 t) 0 s ) 
bred' _ = Nothing

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

bnf' :: DBTerm -> DBTerm
bnf' t = case lred' t of
  Nothing -> t
  Just t' -> bnf' t'

-- Some DB terms

i'       = DBAbs (DBVar 0)
k'       = DBAbs (DBAbs (DBVar 1))
s'       = DBAbs (DBAbs (DBAbs (DBApp
            (DBApp (DBVar 2)(DBVar 0))
            (DBApp (DBVar 1)(DBVar 0))) )) 
b'       = DBAbs (DBVar 2)
true'    = DBAbs (DBAbs (DBVar 1))
false'   = DBAbs (DBAbs (DBVar 0))

and'     = DBAbs (DBAbs (DBAbs (DBAbs (DBApp (DBApp (DBVar 3) (DBApp (DBApp (DBVar 2) (DBVar 1)) (DBVar 0)))(DBVar 0)))))

or'     = DBAbs (DBAbs (DBAbs (DBAbs (DBApp (DBApp (DBVar 3) (DBVar 0))  (DBApp (DBApp (DBVar 2) (DBVar 1)) (DBVar 0))  ))))

not' = DBAbs (DBAbs (DBAbs (DBApp (DBApp (DBVar 2) (DBVar 0)) (DBVar 1))))

y' = DBAbs (DBApp g g) 
  where g = DBAbs $ DBApp (DBVar 1) (DBApp (DBVar 0) (DBVar 0))

w' = DBAbs (DBApp (DBVar 0) (DBVar 0))
omega' = DBApp w' w'

theta' = DBApp x x where
  x =  DBAbs (DBAbs (DBApp  (DBVar 0) (DBApp (DBApp (DBVar 1) (DBVar 1)) (DBVar 0))))

zero'    = DBAbs (DBAbs (DBVar 0))

churchNat' :: Int -> DBTerm
churchNat' 0 = zero'
churchNat' n = DBAbs (DBAbs (DBApp 
  (DBVar 1)
  (DBApp 
    (DBApp (churchNat' (n-1)) (DBVar 1)) 
    (DBVar 0))))

succ'   = DBAbs (DBAbs (DBAbs (DBApp 
            (DBVar 1) 
            (DBApp 
              (DBApp (DBVar 2) (DBVar 1)) 
              (DBVar 0)) ))) 

iszero'  = DBAbs (DBApp 
          (DBApp 
            (DBVar 0) 
            (DBAbs false'))
          (true'))

pred' = DBAbs (DBApp 
  (DBApp 
    (DBApp iszero' (DBVar 0))
    (zero')) 
  (DBApp 
    (DBApp 
      (DBVar 0) 
      (DBAbs (DBApp 
        (DBApp (DBVar 0) (i')) 
        (DBApp (succ') (DBVar 0))))) 
    (DBAbs (DBAbs zero')))) 

rcase' = DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp
    (DBApp iszero' (DBVar 2))
    (DBVar 1)) 
  (DBApp 
    (DBVar 0) 
    (DBApp pred' (DBVar 2)))) ))

equals' = DBApp y' (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp (DBApp iszero' (DBVar 1)) (DBApp iszero' (DBVar 0))) 
  (DBApp 
    (DBApp (DBApp iszero' (DBVar 0)) (false')) 
    (DBApp (DBApp (DBVar 2) (DBApp pred' (DBVar 1))) (DBApp pred' (DBVar 0)))) ))))

add' = DBAbs (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp (DBVar 3) (DBVar 1))
  (DBApp 
    (DBApp (DBVar 2) (DBVar 1)) 
    (DBVar 0))) )))

geq' = DBApp y' (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp 
    (DBApp rcase' (DBVar 1)) 
    (DBApp iszero' (DBVar 0))) 
  (DBAbs (DBApp 
    (DBApp (DBVar 3) (DBVar 0)) 
    (DBApp pred' (DBVar 1)))))))) 



