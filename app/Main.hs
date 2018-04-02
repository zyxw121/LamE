module Main where
import System.Environment
import Core
import Syntax
import Parser
import LamE
import Terms
--import Text.Pretty.Simple

main :: IO ()
main = do
  (path:ps) <- getArgs
  source <- readFile path 
  let prog = parseStrP source
  let res = church . partial $ act' prog prim
  print res
  print $ bnf res
