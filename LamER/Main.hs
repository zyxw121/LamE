{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where
import Core
import Syntax
import Parser
import Eval
import Terms
import Env
import Control.Monad.Trans.Class

-- IO () with access to an Env
loop :: EnvT Action IO ()
loop = do
  input <- lift getLine
  case parseCom input of
    Left e -> lift $ putStrLn e
    Right v -> execute v
  loop

execute :: Command -> EnvT Action IO ()
execute = \case 
  Define d -> elabM d
  Evaluate e -> eval e >>= (\a -> lift $ print a)

main = runEnvT loop prim
 
  
