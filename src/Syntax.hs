module Syntax where
import Core
import Env

type Ident = String

data Expr  = VarExp Name
            | TermExp Term
            | CharExp Char
            | ListExp [Expr]
            | StringExp String
            | BoolExp Bool 
            | NumExp Int
            | If Expr Expr Expr
            | Match Expr (Name, Expr) (Name, Name, Expr) (Name, Name, Expr)
            | Func [Name] Expr
            | Apply Expr [Expr]
            | Let Defn Expr 
            deriving (Eq, Show)

data Defn  = Val Name Expr 
            | Rec Name Expr
            deriving (Eq, Show) 
         
data Program  = Program [Defn] Expr deriving (Eq, Show)

type Env = Environment Action

data Action = Param Name
            | TermAct Term 
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

data Prim = Plus | Minus | Times | Div | Mod 
          | And | Or | Not 
          | Equal | Lesser | Leq | Geq | Greater
          | ChEqual
          | Head | Tail | Cons | Empty
          | StrEqual
          | VAR | APP | ABS
          deriving (Show, Eq) 

data Combinator = CPrim Prim 
                | CInt Int 
                | CBool Bool 
                | CChar Char 
                | CList [Partial Combinator] 
                | CString  String 
                | CTerm Term 
                | Y 
                deriving (Show)

data Partial a = PVar Name 
                | PAbs Name (Partial a) 
                | PApp (Partial a) (Partial a) 
                | Hole a 
                deriving (Show)
