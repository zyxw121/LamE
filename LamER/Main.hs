{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where
import Core
import Syntax
import Parser
import Eval
import Terms
import Env
import Control.Monad.Trans.Class
import System.Exit
import System.IO
import System.FilePath
import Data.List

-- IO () with access to an Env
rep :: EnvT Action IO ()
rep =  prompt  >>= (\case 
  Left e -> lift $ putStrLn e
  Right v -> execute v) . parseCom

execute :: Command -> EnvT Action IO ()
execute = \case 
  Define d -> elabM d
  Evaluate e -> eval e >>= (\a -> lift . print . church . partial $ a)
  Quit -> lift $ putStrLn "Quiting LamER" >> exitSuccess
  Reset -> envM >>= lift . reset >>= inM
  Load path -> loadFile path

loadFile :: String -> EnvT Action IO ()
loadFile path = do
  envs <- envM
  (lift $ envFromFile path) >>= \case
    Just envm -> addM $ Module 
      { module_name = path
      , module_env = envm
      , module_reload = envFromFile path}
    Nothing -> return ()
  
envFromFile :: String -> IO (Maybe (Environment Action))
envFromFile path = do
  file <- readFile path
  case parseProg file of
    Left e -> putStrLn e >> return Nothing
    Right (Program ds e) -> return . Just $ foldr elab1 new_env_ (reverse ds)

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

main = putStrLn "LamER, version 1.0.1" >> runEnvT (loopM rep) prim
