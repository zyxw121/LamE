module Env where
import Core
import Control.Monad
import Control.Monad.Trans.Class

type Environment a =  [(Name,a)] -- Mapping names to as

newtype EnvT v m a = EnvT {runEnvT :: Environment v -> m (a, Environment v) }

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

findM :: Monad m => Name -> EnvT v m (Maybe v)
findM n = EnvT (\e -> return (find e n, e))

defineM :: Monad m => Name -> v -> EnvT v m ()
defineM x v = EnvT (\e -> return((), define e x v ))
  
defsM :: Monad m => [(Name, v)] -> EnvT v m ()
defsM xs = EnvT (\e -> return ((), defs xs e))

envM :: Monad m => EnvT v m (Environment v)
envM = EnvT (\e -> return (e,e))

inM :: Monad m => Environment v -> EnvT v m ()
inM env = EnvT (\e -> return ((), env))

find :: Environment a -> Name -> Maybe a
find env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define :: Environment a -> Name -> a -> Environment a
define env x v = (x,v):env

defs :: Environment a -> [(Name, a)] -> Environment a
defs = flip (++) 

defargs :: Environment a -> [Name] -> [a] -> Environment a
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a


