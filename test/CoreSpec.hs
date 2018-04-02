module CoreSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Syntax
import Parser
import Core
import Terms
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)

smaller a b c= modifyMaxSize (const a) $ modifyMaxSuccess (const b) $ c

instance Arbitrary Name where
  arbitrary = (chars 1) >>= return . Name

shorts :: Gen String
shorts = do
  n <- getSize
  let m = 1 + n
  let xs = zip (reverse [1..m]) (map chars [1..m] ) 
  frequency xs

chars :: Int -> Gen String
chars 1 = char' >>= return . (:[])
chars n = do
  c <- char'
  cs <- chars (n-1)
  return (c:cs)

char' :: Gen Char
char' = do
  n <- choose (0,51)
  return $ alloweds !! n

instance Arbitrary Term where
  arbitrary = getSize >>= termN 
    
termN :: Int -> Gen Term
termN 0 = arbitrary >>= return . Var
termN n = do
  x <- arbitrary
  if x 
    then do
      s <- termN (n `div` 2)
      t <- termN (n `div` 2)
      return $ App s t
    else do
      s <- termN (n-1)
      z <- arbitrary
      return $ Abs z s

equ s t = (toDB s) == (toDB t)

spec :: Spec
spec = do
  describe "news" $ do
    smaller 1000 100 $ it "works" $ property $
      \xs -> not (fresh' xs `elem` xs) 

  describe "substitution" $ do
    smaller 100 1000 $ it "works" $ property $ 
      \x y s t u -> ((x/=y) && (not (x`elem` (frees u)))) ==> equ (sub (sub (s) (t) ( x)) (u) ( y)) ( sub (sub (s) (u) ( y)) (sub (t) (u) ( y)) ( x) )
    smaller 100 1000 $ it "sometimes" $ property $
      \x s t -> not (x `elem` frees s) ==> equ (sub s t x)  s
