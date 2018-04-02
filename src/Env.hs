module Env where
import Core

type Environment a =  [(Name,a)] -- Mapping names to as

find :: Environment a -> Name -> Maybe a
find env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define :: Environment a -> Name -> a -> Environment a
define env x v = (x,v):env

defargs :: Environment a -> [Name] -> [a] -> Environment a
defargs env [] [] = env
defargs env (x:xs) (a:as) = define (defargs env xs as) x a

