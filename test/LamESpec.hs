module LamESpec where

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

