module Syntax where
import Core

type Ident = String

data Expr a = VarExp a
            | NumExp Integer
            | If (Expr a) (Expr a) (Expr a)
            | Func [Ident] (Expr a)
            | Apply (Expr a) [Expr a]
            | Let (Defn a) (Expr a) 
            deriving (Show)

data Defn a = Val a (Expr a) 
            | Rec a (Expr a)
            deriving (Show) 
         
data Program a = Program [Defn a] (Expr a) deriving (Show)
