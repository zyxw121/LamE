module Terms where
import Core
import Prelude hiding (abs, and, or, fst, snd)
import Data.List (unfoldr)

--Shortcuts
abs = Abs . Name
v = Var . Name

app2 a b c = App (App a b) c

abss :: String -> Term -> Term
abss [x] t = abs (x:[]) t
abss (x:y:xs) t = abs (x:[]) $ abss (y:xs) t 

-- Misc Combinators

i = abs "x" (v"x")
s = abss "abc" (App (App (v "a") (v "c")) (App (v "b") (v "c")))
k = abss "xy" (v "y")

y = abs "f" (App x x)
  where x = abs "x" (App (v "f") (App (v"x") (v"x"))) 

-- Booleans

true = abs "a" (abs "b" $ v"a" )
false = abs "a" (abs "b" $ v"b" )

and = abss "xyab" (App (App (v"x") (App (App (v"y") (v"a")) (v"b"))) (v"b")) 
or = abss "xyab" (App (App (v "x") (v"a")) ( (App (App (v"y") (v"a")) (v"b")) )) 
neg = abss "xab" (App (App (v "x") (v"b"))(v"a"))
_and = app2 and 
_or  = app2 or 
_neg = App neg  

churchBool :: Bool -> Term
churchBool True = true
churchBool False = false

unBool' :: Term -> Bool
unBool' s | toDB s == true' = True
         | toDB s == false' = False
         | otherwise = error "not boolean"

unBool = unBool' . bnf

-- Nats

zeroNat = abss "fx" (v "x")

addNat = abss "mnfx" (App (App (v"m") (v "f")) (App (App (v "n") (v "f")) (v "x"))) 
multNat = abss "mnfx" (App (App (v "m") (App (v "n") (v "f"))) (v "x"))
succNat = abss "nfx" (App (v "f") (App (App (v "n") (v "f")) (v "x")))
iszero = abs "n" (App (App (v "n") (abs "x" false)) true)
predNat = abs "n" (_iszero (v "n") (zeroNat) 
  (App (App (v "n") (abs "y" 
    (App (App (v "y") (i)) (App (succNat) (v "y"))) )) 
    (abss "ab" zeroNat)))
--Better version of predNat
predNat1 = abss "nfx" (App (App (App (v "n") (abss "gh" (App (v "h") (App (v "g") (v "f"))))) (abs "u" (v "x"))) (i))
minusNat = abss "nm" (App (App (v "m") predNat1 ) (v "n"))

divmodNat = app2 y (abss "gqab" (app2 
  (_lesserNat (v"a") (v"b") ) 
  (pair (v"q",v"a")) 
  (App (app2 (v"g") (App succNat (v"q")) (_minusNat (v"a") (v"b"))) (v"b")))) (zeroNat)
_divmodNat = app2 divmodNat

divmod n m = (n `div` m, n`mod` m)

divmod' n m = let x = app2 divmodNat (churchNat n) (churchNat m) in let (n',m') = (_fst x, _snd x) in (unNat n', unNat m')


_addNat = app2 addNat 
_multNat = app2 multNat 
_iszero n f g = App (App (App iszero n) f) g
_minusNat = app2 minusNat 
_predNat = App predNat

leqNat = abss "nm" (App iszero (_minusNat (v "n") (v "m")))
eqNat = abss "nm" (_and (_leqNat (v "n") (v "m")) (_leqNat (v"m") (v"n")))

_leqNat = app2 leqNat 
_lesserNat a b =  (_neg (_leqNat (b) (a))) 
_eqNat = app2 eqNat 

churchNat :: Int -> Term
churchNat n = if n < 0 then error "nats can't be negative..." else  abss "fx" $ foldr (App) (v "x") (take n $ repeat (v"f"))

unApp :: Term -> Maybe (Name, Term)
unApp (App (Var x) s) = Just (x, s)
unApp _ = Nothing

unNat' :: Term -> Int 
unNat' (Abs f (Abs x t)) = let ns =  unfoldr unApp t in if foldr (&&) (True) .  map (==f) $  ns then length ns else error "not Nat"
unNat' _ = error "not Nat"

unNat = unNat' . bnf
--unNat' :: Term -> Int -> Int
--unNat' s n | toDB s == bnf' (churchNat' n) = n
--           | otherwise = unNat' s (n+1)
--unNat s = unNat' s 0

absdiff = abss "nm" $ _addNat (_minusNat (v "n") (v"m")) (_minusNat (v"m") (v"n"))
_absdiff = app2 absdiff
eqNat1 = abss "nm" $ App iszero (_absdiff (v "n") (v "m")) 
_eqNat1 = app2 eqNat1

-- Pairs/Cons

pair :: (Term, Term) -> Term 
pair (x,y) = abs "z" (App (App (v "z") (x)) (y))

fst = abs "p" (App (v "p")(true))
snd = abs "p" (App (v "p")(false))
_fst = App fst 
_snd = App snd  

unPair :: Term -> (Term, Term)
unPair s = (fromDB . bnf' . toDB . _fst $ s, fromDB . bnf' . toDB . _snd $s )
-- Ints

uminus = abs "n" $ pair (App snd (v "n"), App fst (v "n"))
addInt = abss "nm" $ pair (_addNat (_fst (v "n")) (_fst (v "m")) , _addNat (_snd (v"n")) (_snd (v"m")) )
minusInt = abss "nm" $ _addInt (v "n") (_uminus (v "m"))
timesInt = abss "nm" $ pair ( _addNat (_multNat (_fst n) (_fst m)) (_multNat (_snd n) (_snd m))   , _addNat (_multNat (_fst n) (_snd m)) (_multNat (_fst m) (_snd n)) ) where 
  n = v "n"
  m = v "m"


_uminus  = App uminus 
_addInt = app2 addInt 
_minusInt = app2 minusInt 
_timesInt = app2 timesInt 


lesserInt = abss "nm" $ _neg (_leqInt (v "m") (v "n"))
leqInt = abss "nm" (_leqNat (_addNat (_fst $ v "n") (_snd $ v"m")) (_addNat (_fst $ v"m") (_snd $ v"n"))) 
equalInt = abss "nm" $ _and (_leqInt (v "n") (v "m")) (_leqInt (v "m") (v "n")) 
geqInt = abss "nm" $ _leqInt (v"m") (v "n")
greaterInt = abss "nm" $  _lesserInt (v "m") (v "n")

_lesserInt = app2 lesserInt
_leqInt = app2 leqInt 
_equalInt = app2 equalInt 
_geqInt = app2 geqInt
_greaterInt = app2 greaterInt

toNat = abs "n" $ _absdiff (_fst (v "n")) (_snd (v "n"))
_toNat = App toNat
toInt = abs "n" $ pair (v"n", zeroNat)
_toInt = App toInt

negative = abs "n" $ _lesserNat (_fst (v"n")) (_snd (v"n"))
_negative = App negative

sign = abs "n" $ App (app2 negative (v"n") (churchInt (-1))) (churchInt (1))
_sign = App sign

--careful with negatives
divmodInt = let z= _divmodNat (_toNat (v"n")) (_toNat (v"m")) in abss "nm" $ pair (_timesInt (_toInt (_fst z)) (_sign (v"n")) ,_timesInt (_timesInt (_sign (v"n")) (_sign (v"m"))) (_toInt (_snd z)) )
_divmodInt = app2 divmodInt

divMod' n m = let z = _divmodInt (churchInt n) (churchInt m) in (unInt . _fst $z, unInt . _snd $z)

divideInt = abss "nm" $ _fst (_divmodInt (v"n") (v"m"))
modInt = abss "nm" $ _snd (_divmodInt (v"n") (v"m"))

churchInt :: Int -> Term
churchInt x | x >= 0 = pair (churchNat x, zeroNat)
            | otherwise = pair (zeroNat, churchNat (-x))

unInt :: Term -> Int
unInt s = let (a,b) = unPair s in (unNat a) - (unNat b)



-- Char, (just Nats really)

equalChar = eqNat 
_equalChar = app2 equalChar

churchChar :: Char -> Term
churchChar = churchNat . fromEnum

unChar :: Term -> Char
unChar = toEnum . unNat 

-- Lists

nil = abs "x" true
cons = abss "xy" $ pair (v"x",v"y")
headT = fst
tailT = snd
emptyList = abs "p" $ App (v "p") (abss "xy" false)

_cons = app2 cons
_headT = App headT
_tailT = App tailT
_emptyList = App emptyList

churchList :: [Term] -> Term
churchList [] = nil
churchList (x:xs) = _cons x (churchList xs) 

unList :: Term -> [Term]
unList l = if unBool (bnf . _emptyList $l) then [] else (_headT l):(unList (_tailT l))

-- Strings (lists of chars)

equalString = App y (abss "exy" ( app2 (_emptyList (v"x"))  
  (_emptyList (v "y") )  
  (app2 (_emptyList (v"y")) 
    (false ) 
    (_and 
      (_equalChar (_headT (v"x")) (_headT (v"y"))) 
      (app2 (v "e") (_tailT (v"x")) (_tailT (v"y"))) ))))

_equalString = app2 equalString

churchString :: String -> Term
churchString = churchList . map churchChar

unString = map unChar . unList

-- Terms

churchTerm (Var (Name n)) = abss "abc" $ App (v"a") (churchString n)
churchTerm (App s t) = abss "abc" $ app2 (v"b") (churchTerm s) (churchTerm t)
churchTerm (Abs (Name n) s) = abss "abc" $ app2 (v "c") (churchString n) (churchTerm s)

varT = abss "xabc" $ App (v"a") (v"x")
appT = abss "stabc" $ app2 (v"b") (v"s") (v"t")
absT = abss "xsabc" $ app2 (v"c") (v"x") (v"s")

