module TermSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Syntax
import Parser
import Core
import Terms
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)

tInt :: (Term -> Term -> Term) -> Int -> Int -> Int
tInt t a b = unchurch . bnf $ t (church a) (church b)

tIntP :: (Term -> Term -> Term) -> Int -> Int -> Bool
tIntP t a b = unchurch . bnf $ t (church a) (church b)

smaller a b c= modifyMaxSize (const a) $ modifyMaxSuccess (const b) $ c

spec :: Spec
spec = do
  describe "Ints" $ do
    smaller 100 50 $ it "converts" $ property $ 
      \x -> (unchurch . church) x == (x::Int)
    smaller 50 50 $ it "adds" $ property $ 
      \x y -> tInt _addInt x y == ( (x::Int) + (y::Int) )
    smaller 50 50 $ it "subtracts" $ property $ 
      \x y -> tInt _minusInt x y == ( (x::Int) - (y::Int) )
    smaller 50 50 $ it "multiplies" $ property $ 
      \x y -> tInt _timesInt x y == ( (x::Int) * (y::Int) )
    smaller 50 50 $ it "lessers" $ property $ 
      \x y -> tIntP _lesserInt x y == ( (x::Int) < (y::Int) )
    smaller 50 50 $ it "leqs" $ property $ 
      \x y -> tIntP _leqInt x y == ( (x::Int) <= (y::Int) )
    smaller 50 50 $ it "equals" $ property $ 
      \x y -> tIntP _equalInt x y == ( (x::Int) == (y::Int) )
    smaller 50 50 $ it "geqs" $ property $ 
      \x y -> tIntP _geqInt x y == ( (x::Int) >= (y::Int) )
    smaller 50 50 $ it "greaters" $ property $ 
      \x y -> tIntP _greaterInt x y == ( (x::Int) > (y::Int) ) 
  describe "Bools" $ do
    it "ands" $ do 
      (unchurch $ (_and (church True) (church True)))  `shouldBe` True
      (unchurch $ (_and (church True) (church False)))  `shouldBe` False
      (unchurch $ (_and (church False) (church True)))  `shouldBe` False
      (unchurch $ (_and (church False) (church False)))  `shouldBe` False
    it "ors" $ do 
      (unchurch $ (_or (church True) (church True)))  `shouldBe` True
      (unchurch $ (_or (church True) (church False)))  `shouldBe` True
      (unchurch $ (_or (church False) (church True)))  `shouldBe` True
      (unchurch $ (_or (church False) (church False)))  `shouldBe` False
    it "nots" $ do 
      (unchurch . bnf $ (_neg (church True)))  `shouldBe` False
      (unchurch . bnf $ (_neg (church False))) `shouldBe` True
  describe "Chars" $ do
    smaller 50 50 $ it "converts" $ property $ 
      \x -> (unchurch . church) x == (x::Char) 
  describe "Lists" $ do
    it "has empty" $ do
      (unchurch  . church) (tail [0::Int]) `shouldBe` (tail [0::Int]) 
      (unchurch  . _emptyList . church) ([]::[Int]) `shouldBe` True 
      (unchurch  . _emptyList . church) ([1]::[Int]) `shouldBe` False 
    smaller 50 50 $ it "converts Ints" $ property $ 
      \xs -> ( unchurch . church) xs == (xs :: [Int])
    smaller 50 50 $ it "converts Bools" $ property $ 
      \xs -> (unchurch . church) xs == (xs :: [Bool])
    smaller 30 50 $ it "conses" $ property $ 
      \x xs -> (unchurch . bnf $ _equalInt (church (x:: Int)) (_headT (_cons (church (x::Int)) (church (xs::Int))) )) && ( unchurch . bnf $ _equalInt (church (xs::Int)) (_tailT (_cons (church (x::Int))(church (xs::Int))))     )
  describe "Strings" $ do
    smaller 5 25 $ it "converts" $ property $ 
      \s -> (unchurch . church) s == (s :: String) 


