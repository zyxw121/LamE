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


-- Reduction
--shift' n m t increments all vars in t that are > than m by n
shift' :: Integer -> Integer -> DBTerm -> DBTerm
shift' n m (DBVar k) = if k >= m then (DBVar $ n+k) else (DBVar k)
shift' n m (DBApp u v) = DBApp (shift' n m u) (shift' n m v)
shift' n m (DBAbs s) = DBAbs (shift' n (m+1) s)

--shift n incs free vars by n
shift :: Integer -> DBTerm -> DBTerm
shift n = shift' n 0

dbSub :: DBTerm -> Integer -> DBTerm -> DBTerm
dbSub t n (DBVar m) = if n==m then t else (DBVar m)
dbSub t n (DBApp u v) = DBApp (dbSub t n u) (dbSub t n v)
dbSub t n (DBAbs s) = DBAbs (dbSub (shift 1 t) (n+1) s ) 

dbBetaRed :: DBTerm -> Maybe DBTerm
dbBetaRed (DBApp (DBAbs s) t) = Just $ shift (-1) (dbSub (shift 1 t) 0 s ) 
dbBetaRed _ = Nothing

dbLeftRed :: DBTerm -> Maybe DBTerm
dbLeftRed (DBVar x) = Nothing
dbLeftRed u@(DBApp s t) = dbBetaRed u 
  <|> do
    x <- dbLeftRed s
    return $ DBApp x t
  <|> do 
    x <- dbLeftRed t 
    return $ DBApp s x
lred (DBAbs s) = dbLeftRed s >>= return . DBAbs

dbBetaNormal :: DBTerm -> DBTerm
dbBetaNormal t = case dbLeftRed t of
  Nothing -> t
  Just t' -> dbBetaNormal t'

-- Some DB terms

i       = DBAbs (DBVar 0)
k       = DBAbs (DBAbs (DBVar 1))
s       = DBAbs (DBAbs (DBAbs (DBApp
            (DBApp (DBVar 2)(DBVar 0))
            (DBApp (DBVar 1)(DBVar 0))) )) 
b       = DBAbs (DBVar 2)
true    = DBAbs (DBAbs (DBVar 1))
false   = DBAbs (DBAbs (DBVar 0))


y = DBAbs (DBApp g g) 
  where g = DBAbs $ DBApp (DBVar 1) (DBApp (DBVar 0) (DBVar 0))

w = DBAbs (DBApp (DBVar 0) (DBVar 0))
omega = DBApp w w

theta = DBApp x x where
  x =  DBAbs (DBAbs (DBApp  (DBVar 0) (DBApp (DBApp (DBVar 1) (DBVar 1)) (DBVar 0))))

zero    = DBAbs (DBAbs (DBVar 0))

church :: Integer -> DBTerm
church 0 = zero
church n = DBAbs (DBAbs (DBApp 
  (DBVar 1)
  (DBApp 
    (DBApp (church (n-1)) (DBVar 1)) 
    (DBVar 0))))

succ'   = DBAbs (DBAbs (DBAbs (DBApp 
            (DBVar 1) 
            (DBApp 
              (DBApp (DBVar 2) (DBVar 1)) 
              (DBVar 0)) ))) 

iszero  = DBAbs (DBApp 
          (DBApp 
            (DBVar 0) 
            (DBAbs false))
          (true))

pred' = DBAbs (DBApp 
  (DBApp 
    (DBApp iszero (DBVar 0))
    (zero)) 
  (DBApp 
    (DBApp 
      (DBVar 0) 
      (DBAbs (DBApp 
        (DBApp (DBVar 0) (i)) 
        (DBApp (succ') (DBVar 0))))) 
    (DBAbs (DBAbs zero)))) 

rcase = DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp
    (DBApp iszero (DBVar 2))
    (DBVar 1)) 
  (DBApp 
    (DBVar 0) 
    (DBApp pred' (DBVar 2)))) ))

equals = DBApp y (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp (DBApp iszero (DBVar 1)) (DBApp iszero (DBVar 0))) 
  (DBApp 
    (DBApp (DBApp iszero (DBVar 0)) (false)) 
    (DBApp (DBApp (DBVar 2) (DBApp pred' (DBVar 1))) (DBApp pred' (DBVar 0)))) ))))

add = DBAbs (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp (DBVar 3) (DBVar 1))
  (DBApp 
    (DBApp (DBVar 2) (DBVar 1)) 
    (DBVar 0))) )))

geq = DBApp y (DBAbs (DBAbs (DBAbs (DBApp 
  (DBApp 
    (DBApp rcase (DBVar 1)) 
    (DBApp iszero (DBVar 0))) 
  (DBAbs (DBApp 
    (DBApp (DBVar 3) (DBVar 0)) 
    (DBApp pred' (DBVar 1)))))))) 



