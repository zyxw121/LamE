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
  let res = term . partial $ act' prog prim
  print res
       
   
