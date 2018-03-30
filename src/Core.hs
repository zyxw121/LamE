{-# LANGUAGE  RecordWildCards #-}
module Core where
import Control.Applicative
import Prelude hiding (and, or, abs, pred, succ, fst, snd)

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

alloweds = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-+*/<>="

prims = words "== + - * / < > <= >= pred succ and or not"

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

--get things back from bnf terms
churchBool :: Bool -> Term
churchBool True = true
churchBool False = false

unBool :: Term -> Bool
unBool s | toDB s == true' = True
         | toDB s == false' = False
         | otherwise = error "not boolean"

unNat' :: Term -> Int -> Int
unNat' s n | toDB s == bnf' (churchNat' n) = n
           | otherwise = unNat' s (n+1)
unNat s = unNat' s 0

unPair :: Term -> (Term, Term)
unPair s = (fromDB . bnf' . toDB . _fst $ s, fromDB . bnf' . toDB . _snd $s )

unInt :: Term -> Int
unInt s = let (a,b) = unPair s in (unNat a) - (unNat b)

--Some terms
abs = Abs . Name
v= Var . Name

app2 a b c = App (App a b) c

abss :: String -> Term -> Term
abss [x] t = abs (x:[]) t
abss (x:y:xs) t = abs (x:[]) $ abss (y:xs) t 

i = abs "x" (v"x")
s = abss "abc" (App (App (v "a") (v "c")) (App (v "b") (v "c")))
k = abss "xy" (v "y")

y = abs "f" (App x x)
  where x = abs "x" (App (v "f") (App (v"x") (v"x"))) 

true = abs "a" (abs "b" $ v"a" )
false = abs "a" (abs "b" $ v"b" )

and = abss "xyab" (App (App (v"x") (App (App (v"y") (v"a")) (v"b"))) (v"b")) 
_and = app2 and 
or = abss "xyab" (App (App (v "x") (v"a")) ( (App (App (v"y") (v"a")) (v"b")) )) 
_or  = app2 or 
neg = abss "xab" (App (App (v "x") (v"b"))(v"a"))
_neg = App neg  

zeroNat = abss "fx" (v "x")

churchNat :: Int -> Term
churchNat n = if n < 0 then error "nats can't be negative..." else  abss "fx" $ foldr (App) (v "x") (take n $ repeat (v"f"))

addNat :: Term
addNat = abss "mnfx" (App (App (v"m") (v "f")) (App (App (v "n") (v "f")) (v "x"))) 

_addNat = app2 addNat 

multNat :: Term
multNat = abss "mnfx" (App (App (v "m") (App (v "n") (v "f"))) (v "x"))
_multNat = app2 multNat 

succNat = abss "nfx" (App (v "f") (App (App (v "n") (v "f")) (v "x")))

iszero = abs "n" (App (App (v "n") (abs "x" false)) true)

_iszero :: Term -> Term -> Term -> Term
_iszero n f g = App (App (App iszero n) f) g


predNat = abs "n" (_iszero (v "n") (zeroNat) 
  (App (App (v "n") (abs "y" 
    (App (App (v "y") (i)) (App (succNat) (v "y"))) )) 
    (abss "ab" zeroNat)))
_predNat = App predNat

predNat1 = abss "nfx" (App (App (App (v "n") (abss "gh" (App (v "h") (App (v "g") (v "f"))))) (abs "u" (v "x"))) (i))

minusNat = abss "nm" (App (App (v "m") predNat1 ) (v "n"))
_minusNat = app2 minusNat 

leqNat = abss "nm" (App iszero (_minusNat (v "n") (v "m")))
_leqNat = app2 leqNat 

eqNat = abss "nm" (_and (_leqNat (v "n") (v "m")) (_leqNat (v"m") (v"n")))
_eqNat = app2 eqNat 


--encode ordered pairs (a,b) as \z.zab
pair :: (Term, Term) -> Term 
pair (x,y) = abs "z" (App (App (v "z") (x)) (y))

fst = abs "p" (App (v "p")(true))
snd = abs "p" (App (v "p")(false))
_fst = App fst 
_snd = App snd  

churchInt :: Int -> Term
churchInt x | x >= 0 = pair (churchNat x, zeroNat)
            | otherwise = pair (zeroNat, churchNat (-x))

uminus = abs "n" $ pair (App snd (v "n"), App fst (v "n"))
_uminus  = App uminus 

addInt = abss "nm" $ pair (_addNat (_fst (v "n")) (_fst (v "m")) , _addNat (_snd (v"n")) (_snd (v"m")) )
_addInt = app2 addInt 

minusInt = abss "nm" $ _addInt (v "n") (_uminus (v "m"))
_minusInt = app2 minusInt 

timesInt = abss "nm" $ pair ( _addNat (_multNat (_fst n) (_fst m)) (_multNat (_snd n) (_snd m))   , _addNat (_multNat (_fst n) (_snd m)) (_multNat (_fst m) (_snd n)) ) where 
  n = v "n"
  m = v "m"
_timesInt = app2 timesInt 

--test these
equalInt = abss "nm" $ _and (_leqInt (v "n") (v "m")) (_leqInt (v "m") (v "n")) 
_equalInt = app2 equalInt 
lesserInt = abss "nm" $ _neg (_leqInt (v "m") (v "n"))
_lesserInt = app2 lesserInt
leqInt = abss "nm" (_leqNat (_addNat (_fst $ v "n") (_snd $ v"m")) (_addNat (_fst $ v"m") (_snd $ v"n"))) 
_leqInt = app2 leqInt 
geqInt = abss "nm" $ _leqInt (v"m") (v "n")
_geqInt = app2 geqInt
greaterInt = abss "nm" $  _lesserInt (v "m") (v "n")
_greaterInt = app2 greaterInt

predInt = i
succInt = i
divideInt = i

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



