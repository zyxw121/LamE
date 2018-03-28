module Syntax where
import Core

type Ident = String

data Expr  = VarExp Name 
            | NumExp Integer
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
            | NumAct Integer 
            | BoolAct Bool 
            | Closure [Name] Expr Env 
            | DefRec Name Expr Env 
            | Application Action [Action] 
            | Primitive Prim 
            deriving (Show)

type Env = () 
type Prim = ()

