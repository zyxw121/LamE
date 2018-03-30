module TermSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Syntax
import Parser
import Core
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Test.Hspec.QuickCheck (modifyMaxSize)

tInt :: (Term -> Term -> Term) -> Int -> Int -> Int
tInt t a b = unInt . fromDB . bnf' . toDB $ t (churchInt a) (churchInt b)

tIntP :: (Term -> Term -> Term) -> Int -> Int -> Bool
tIntP t a b = unBool . fromDB . bnf' . toDB $ t (churchInt a) (churchInt b)

spec :: Spec
spec = do
  describe "Ints" $ do
    modifyMaxSize (const 100) $it "converts" $ property $ 
      \x -> (unInt . churchInt) x == (x::Int)
    modifyMaxSize (const 50) $ it "adds" $ property $ 
      \x y -> tInt _addInt x y == ( (x::Int) + (y::Int) )
    modifyMaxSize (const 50) $it "subtracts" $ property $ 
      \x y -> tInt _minusInt x y == ( (x::Int) - (y::Int) )
    modifyMaxSize (const 15) $ it "multiplies" $ property $ 
      \x y -> tInt _timesInt x y == ( (x::Int) * (y::Int) )
    modifyMaxSize (const 50) $it "lessers" $ property $ 
      \x y -> tIntP _lesserInt x y == ( (x::Int) < (y::Int) )
    modifyMaxSize (const 50) $it "leqs" $ property $ 
      \x y -> tIntP _leqInt x y == ( (x::Int) <= (y::Int) )
    modifyMaxSize (const 50) $it "equals" $ property $ 
      \x y -> tIntP _equalInt x y == ( (x::Int) == (y::Int) )
    modifyMaxSize (const 50) $it "geqs" $ property $ 
      \x y -> tIntP _geqInt x y == ( (x::Int) >= (y::Int) )
    modifyMaxSize (const 50) $it "greaters" $ property $ 
      \x y -> tIntP _greaterInt x y == ( (x::Int) > (y::Int) )
  describe "Bools" $ do
    it "ands" $ do 
      (unBool . fromDB . bnf' . toDB $ (_and (churchBool True) (churchBool True)))  `shouldBe` True
      (unBool . fromDB . bnf' . toDB $ (_and (churchBool True) (churchBool False)))  `shouldBe` False
      (unBool . fromDB . bnf' . toDB $ (_and (churchBool False) (churchBool True)))  `shouldBe` False
      (unBool . fromDB . bnf' . toDB $ (_and (churchBool False) (churchBool False)))  `shouldBe` False
    it "ors" $ do 
      (unBool . fromDB . bnf' . toDB $ (_or (churchBool True) (churchBool True)))  `shouldBe` True
      (unBool . fromDB . bnf' . toDB $ (_or (churchBool True) (churchBool False)))  `shouldBe` True
      (unBool . fromDB . bnf' . toDB $ (_or (churchBool False) (churchBool True)))  `shouldBe` True
      (unBool . fromDB . bnf' . toDB $ (_or (churchBool False) (churchBool False)))  `shouldBe` False
    it "nots" $ do 
      (unBool . fromDB . bnf' . toDB $ (_neg (churchBool True)))  `shouldBe` False
      (unBool . fromDB . bnf' . toDB $ (_neg  (churchBool False)))  `shouldBe` True


