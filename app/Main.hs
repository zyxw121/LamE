module Main where
import System.Environment
import Core
import Syntax
import Parser
import LamE

main :: IO ()
main = do
  (path:ps) <- getArgs
  source <- readFile path 
  let prog = parseStrP source
  print prog
  print $ act' prog prim
  let res = term . partial $ act' prog prim
  print res
  print $ bnf res
       
   
