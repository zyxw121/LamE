{-# LANGUAGE LambdaCase #-}
module Env where
import Core
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad.IO.Class

type Environment a = [(Name,a)] -- Mapping names to as

data Module a = Module
  { module_name :: String
  , module_env :: Environment a
  , module_reload :: IO (Maybe (Environment a))
  }

instance Eq a => Eq (Module a) where
  n==m = (module_name n == module_name m) && (module_env n == module_env m)

data Environments a = Environments 
  { current_env :: Environment a
  , loaded_modules ::  Map.Map String (Module a)
  }
  deriving Eq

newtype EnvT v m a = EnvT {runEnvT :: Environments v -> m (a, Environments v) }

instance Functor m => Functor (EnvT v  m) where
  fmap f xm = EnvT ( \e -> fmap (\(a,e) -> (f a, e)) $ runEnvT xm e )

instance Monad m => Applicative (EnvT v m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (EnvT v m) where
  return x = EnvT (\e -> return (x,e))
  xm >>= f = EnvT (\e ->  runEnvT xm e >>= (\(a,e') -> runEnvT (f a) e' )  )  

instance MonadTrans (EnvT v) where
  lift xm = EnvT (\e -> xm >>= (\a -> return (a,e)))

instance MonadIO m => MonadIO (EnvT v m) where
  liftIO = lift . liftIO

reload :: Module a -> IO (Maybe (Module a))
reload m = module_reload m >>= \case 
  Just env -> return . Just $ m{module_env=env}
  Nothing -> return Nothing

new_env_ :: Environment a
new_env_ = []

new_env :: Environments a
new_env = Environments
  { current_env = new_env_
  , loaded_modules = Map.empty
  }

envFrom :: Environment a -> Environments a
envFrom env = new_env {current_env=env}

addEnv :: Environments a -> Module a -> Environments a
addEnv envs m= envs{loaded_modules = Map.insert (module_name m) m (loaded_modules envs)}

addM :: Monad m => Module a -> EnvT a m ()
addM m = EnvT (\e -> return ((), addEnv e m))

reset :: MonadIO m => Environments a -> m (Environments a)
reset envs = do
  ms <- liftIO . sequence $ Map.map reload (loaded_modules envs) 
  return $ new_env {loaded_modules = Map.foldr f (Map.empty) ms, current_env = new_env_} 
  where
  f x ys = case x of
    Nothing -> ys
    Just y -> Map.insert (module_name y) y ys

findM :: Monad m => Name -> EnvT v m (Maybe v)
findM n = EnvT (\e -> return (find e n, e))

defineM :: Monad m => Name -> v -> EnvT v m ()
defineM x v = EnvT (\e -> return((), define e x v ))
  
defsM :: Monad m => [(Name, v)] -> EnvT v m ()
defsM xs = EnvT (\e -> return ((), defs e xs))

envM :: Monad m => EnvT v m (Environments v)
envM = EnvT (\e -> return (e,e))

inM :: Monad m => Environments v -> EnvT v m ()
inM env = EnvT (\e -> return ((), env))

listM :: Monad m => EnvT v m [String]
listM = EnvT (\e ->return (Map.keys . loaded_modules $ e  ,e) )

find :: Environments a -> Name -> Maybe a
find envs x = let 
  y = find1 (current_env envs) x 
  ys = map (\m -> find1 (module_env m) x) . Map.elems . loaded_modules $ envs in
  foldr (<|>) (Nothing) (y:ys)

define :: Environments a -> Name -> a -> Environments a
define envs x v = envs{current_env = define1 (current_env envs) x v}
   
defs :: Environments a -> [(Name, a)] -> Environments a
defs envs ds = envs{current_env = defs1 (current_env envs) ds}

defargs :: Environments a -> [Name] -> [a] -> Environments a
defargs envs [] [] = envs
defargs envs (n:ns) (a:as) = defargs (define envs n a) ns as 

find1 :: Environment a -> Name -> Maybe a
find1 env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define1 :: Environment a -> Name -> a -> Environment a
define1 env x v = (x,v):env

defs1 :: Environment a -> [(Name, a)] -> Environment a
defs1 = flip (++) 
