{-# LANGUAGE  RecordWildCards #-}
module Core where
import Control.Applicative

data Name = Name {name :: Char, num :: Int }  deriving (Eq)
data Term = Var Name | Abs Name Term | App Term Term

data DBTerm = DBVar Integer | DBAbs DBTerm | DBApp DBTerm DBTerm

-- 0 <-> a_0, 1 <-> b_0, ... etc
instance Enum Name where 
  fromEnum (Name{..}) = ((fromEnum (name::Char))-96) + 26*num
  toEnum n = Name {
    name= toEnum (((n-1)`mod`26) +97), 
    num=n `div` 26}


--Conversions between term types
frees :: Term -> [Name]
frees (Var n) = [n]
frees (App s t) = frees s ++ frees t
frees (Abs x s) = filter (/=x) $ frees s

index :: (Eq a) => [a] -> a -> Integer
index [] a = error "no index"
index (x:xs) a = if x==a then 0 else 1+(index xs a)

toDB' :: Term -> [Name] -> DBTerm
toDB' (Var n) ns = DBVar $ index ns n 
toDB' (App s t) ns = DBApp (toDB' s ns) (toDB' t ns)
toDB' (Abs x s) ns = DBAbs (toDB' s (x:ns))

toDB :: Term -> DBTerm
toDB t = toDB' t (frees t) 


smallest :: [Int] -> Int
smallest xs = foldr min (length xs+1) [x| x<- [1..length xs +1], not $x `elem` xs ]

fresh :: [(a,Name)] -> Name
fresh es = let (as,names) = unzip es in toEnum . smallest . map fromEnum $ names 

find' :: (Eq a) => [(a,Name)] -> a -> Name
find' es n = let matches = filter (\(x,s) -> x==n) es in case matches of
  [] -> fresh es 
  ((x,s):xs) -> s

fromDB' :: [(Integer, Name)] -> DBTerm -> Term
fromDB' es (DBVar n) = Var (find' es n)
fromDB' es (DBApp s t) = App (fromDB' es s) (fromDB' es t)
fromDB' es (DBAbs s) = let x = fresh es in Abs x (fromDB' ((0,x):(map (\(a,b) -> (a+1,b)) es)) s)

fromDB :: DBTerm -> Term
fromDB t = fromDB' [] t

--Pretty printing
instance Show Name where
  show (Name{..}) = if num>0 then [name]++"_"++(show num) else [name] 

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

