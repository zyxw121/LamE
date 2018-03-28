module Syntax where
import Core

type Ident = String

data Expr  = VarExp Name
            | BoolExp Bool 
            | NumExp Int
            | If Expr Expr Expr
            | Func [Name] Expr
            | Apply Expr [Expr]
            | Let Defn Expr 
            deriving (Show)

data Defn  = Val Name Expr 
            | Rec Name Expr
            deriving (Show) 
         
data Program  = Program [Defn] Expr deriving (Show)

data Action = Param Name 
            | NumAct Int 
            | BoolAct Bool 
            | Closure [Name] Expr Env 
            | DefRec Name Expr Env 
            | Application Action [Action] 
            | Primitive Prim 
            deriving (Show)

data Prim = Plus | Minus | Times | Divide | Pred | Succ
          | And | Or | Not 
          | Equal | Lesser | Leq | Geq | Greater
          deriving (Show) 

data Combinator = CPrim Prim | CInt Int | CBool Bool | Y

data Partial a = PVar Name | PAbs Name (Partial a) | PApp (Partial a) (Partial a) | Hole a


type Environment a =  [(Name,a)] -- Mapping names to as
type Env = Environment Action

find :: Environment a -> Name -> Maybe a
find env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define :: Environment a -> Name -> a -> Environment a
define env x v = (x,v):env

prim :: Env
prim = []
