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
rep :: EnvT Action IO ()
rep = lift getLine >>= (\case 
  Left e -> lift $ putStrLn e
  Right v -> execute v) . parseCom

execute :: Command -> EnvT Action IO ()
execute = \case 
  Define d -> elabM d
  Evaluate e -> eval e >>= (\a -> lift . print  $ a)

loopM :: Monad m=> m a -> m a
loopM xm = xm >> loopM xm

main = runEnvT (loopM rep)  prim
