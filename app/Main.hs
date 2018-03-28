module Main where
import Core
import Syntax
import Parser
import LamE

main :: IO ()
main = do
  source <- readFile "source.lm"
  let prog = parseStrP source
  let res = term . partial $ act' prog prim
  print res
       
   
