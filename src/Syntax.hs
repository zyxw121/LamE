module Syntax where
import Core



type Ident = String

data Expr = VarExp Ident
          | NumExp Integer
          | If Expr Expr Expr
          | Func [Ident] Expr
          | Apply Expr [Expr]
          | Infix Expr Ident Expr
          | Let Defn Expr 
          deriving (Show)

data Defn = Val Ident Expr 
          | Rec Ident Expr
          deriving (Show) 
         
data Program = Program [Defn] Expr
