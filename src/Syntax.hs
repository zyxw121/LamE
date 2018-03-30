module Syntax where
import Core

type Ident = String

data Expr  = VarExp Name
            | CharExp Char
            | ListExp [Expr]
            | StringExp String
            | BoolExp Bool 
            | NumExp Int
            | If Expr Expr Expr
            | Func [Name] Expr
            | Apply Expr [Expr]
            | Let Defn Expr 
            deriving (Eq, Show)

data Defn  = Val Name Expr 
            | Rec Name Expr
            deriving (Eq, Show) 
         
data Program  = Program [Defn] Expr deriving (Eq, Show)

data Action = Param Name 
            | CharAct Char
            | ListAct [Action]
            | StringAct String
            | BoolAct Bool 
            | NumAct Int 
            | Closure [Name] Expr Env 
            | DefRec Name Expr Env 
            | Application Action [Action] 
            | Primitive Prim 
            deriving (Show, Eq)

data Prim = Plus | Minus | Times -- | Divide | Pred | Succ
          | And | Or | Not 
          | Equal | Lesser | Leq | Geq | Greater
          | ChEqual
          | Head | Tail | Cons | Empty
          | StrEqual
          deriving (Show, Eq) 

data Combinator = CPrim Prim | CInt Int | CBool Bool | CChar Char | CList [Partial Combinator] | CString  String | Y deriving (Show)

data Partial a = PVar Name | PAbs Name (Partial a) | PApp (Partial a) (Partial a) | Hole a deriving (Show)

type Environment a =  [(Name,a)] -- Mapping names to as
type Env = Environment Action

find :: Environment a -> Name -> Maybe a
find env x = case (filter (\(a,b) -> a==x) env) of
  [] -> Nothing
  ((a,b):xs) -> Just b 

define :: Environment a -> Name -> a -> Environment a
define env x v = (x,v):env

prims :: Env
prims = map (\(n,p) -> (Name n, Primitive p)) 
  [ ("+", Plus)
  , ("-", Minus)
  , ("*", Times)
  , ("and", And)
  , ("or", Or)
  , ("not", Not)
  , ("==", Equal)
  , ("<", Lesser)
  , ("<=", Leq)
  , (">=", Geq)
  , (">", Greater)
  , ("=c", ChEqual)
  , ("head", Head)
  , ("tail", Tail)
  , ("cons", Cons)
  , ("empty", Empty)
  , ("=s", StrEqual) ]

consts = map (\(n,a) -> (Name n, a)) 
  [("nil", ListAct []) ]

prim = prims ++ consts
