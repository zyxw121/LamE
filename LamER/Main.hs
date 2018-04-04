{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where
import Core
import Syntax
import Parser
import Eval
import Terms
import Env
import Util
import Control.Monad.Trans.Class
import System.Exit
import System.IO
import System.FilePath
import Data.List

-- IO () with access to an Env
rep :: EnvT Action IO ()
rep =  prompt  >>= execute . parseCom

execute :: Command -> EnvT Action IO ()
execute = \case 
  Define d -> elabM d
  Evaluate e p -> compile e >>= (lift . putStrLn . (process p))
  Quit -> lift $ putStrLn "Quiting LamER" >> exitSuccess
  Reset -> envM >>= lift . reset >>= inM
  Load path -> loadFile path
  Bad e c -> lift $ putStrLn e

loopM :: Monad m=> m a -> m a
loopM xm = xm >> loopM xm

prompt ::  EnvT Action IO String 
prompt = do
  ms <- listM 
  lift $ prompt' (intercalate " " (map takeFileName ms) ++ "> ")

prompt' :: String -> IO String
prompt' text = do
    putStr text
    hFlush stdout
    getLine 

main = putStrLn "LamER, version 1.1.2" >> runEnvT (loopM rep) prim
