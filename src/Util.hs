{-# LANGUAGE LambdaCase #-}
module Util where
import Terms
import Core
import Env
import Syntax
import Parser
import Eval
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

process :: Process -> Term -> String
process Bnf = show . bnf
process Hnf = show . hnf
process ToInt = show . (unchurch :: Term -> Int)
process ToBool = show . (unchurch :: Term -> Bool)
process ToChar = show . (unchurch :: Term -> Char)
process ToString = show . (unchurch :: Term -> String)
process None = show

loadFile :: MonadIO m => String -> EnvT Action m ()
loadFile path = do
  envs <- envM
  (liftIO $ envFromFile path) >>= \case
    Just envm -> addM $ Module 
      { module_name = path
      , module_env = envm
      , module_reload = envFromFile path}
    Nothing -> return ()
  
envFromFile :: MonadIO m => String -> m (Maybe (Environment Action))
envFromFile path = do
  file <- liftIO $ readFile path
  case parseProg file of
    Left e -> (liftIO $ putStrLn e) >> return Nothing
    Right (Program ds e) -> return . Just $ foldr elab1 new_env_ (reverse ds)

compile :: Monad m => Expr -> EnvT Action m Term
compile e = eval e >>= (return . church . partial)
