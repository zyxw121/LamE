module Syntax where
import Core
import Env
import Data.List (intercalate)

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

data Defn  = Val Name Expr 
            | Rec Name Expr
         
data Program  = Program [Defn] Expr 

type Env = Environments Action

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

data Prim = Plus | Minus | Times | Div | Mod 
          | And | Or | Not 
          | Equal | Lesser | Leq | Geq | Greater
          | ChEqual
          | Head | Tail | Cons | Empty
          | StrEqual
          | VAR | APP | ABS
          deriving (Show) 

data Combinator = CPrim Prim 
                | CInt Int 
                | CBool Bool 
                | CChar Char 
                | CList [Partial Combinator] 
                | CString  String 
                | CTerm Term 
                | Y 

data Partial a = PVar Name 
                | PAbs Name (Partial a) 
                | PApp (Partial a) (Partial a) 
                | Hole a 

data Command = Define Defn 
             | Evaluate Expr 
             | Reset 
             | Quit
             | Load String
             | Final (Term -> String)
-- Pretty printing

pvars :: Partial a -> [Name]
pvars (PAbs n s) = n:(pvars s)
pvars _ = []

pbody :: Partial a -> Partial a 
pbody (PAbs n s) = pbody s
pbody t = t

ppp :: (Show a) => Int -> Partial a -> String
ppp r (PVar n) = show n
ppp r (PApp s t) = addParens r $ ppp (r+1) s ++ " " ++  ppp r t
ppp r t@(PAbs n s) = let (vs, b) = (map show $ pvars t, pbody t) in addParens r $ "\\" ++ (intercalate " " vs) ++"." ++ ppp (r+1) b  
ppp r (Hole a) = show a

instance (Show a) => Show (Partial a) where
  show = ppp 0 

instance Show Combinator where
  show c = case c of
    (CPrim p) -> show p 
    (CInt n) -> show n
    (CBool b) -> if b then "⊤" else "⊥"
    (CChar c) -> ['\'',c,'\'']
    (CList xs) -> "[" ++ intercalate ", " (map show xs) ++ "]"
    (CString s) -> "\"" ++ s ++ "\""
    (CTerm t) -> "{" ++ show t ++ "}"
    (Y) -> "Y"
instance Show Defn where
  show d = case d of
    (Val x e) -> "Val " ++ show x ++ " = " ++ show e
    (Rec x e) -> "Rec " ++ show x ++ " = " ++ show e

instance Show Program where
  show (Program ds e) = intercalate "\n" (map show ds) ++ "\n" ++ show e

instance Show Expr where
  show e = case e of
    (VarExp n) -> "Var " ++ show n 
    (TermExp t) -> "Term" ++ show t
    (CharExp c) ->  ['\'',c,'\'']
    (ListExp es) -> "[" ++ intercalate ", " (map show es) ++ "]"
    (StringExp s) -> "\"" ++ s ++ "\""
    (BoolExp b) -> show b
    (NumExp n) -> show n
    (If e1 e2 e3) -> "If " ++ addParens 1 (show e1) ++ addParens 1 (show e2) ++ addParens 1 (show e3) 
    (Match e (x,e1) (s,t,e2) (y,u,e3)) -> undefined
    (Func ns e) -> "Func " ++ "[" ++ intercalate ", " (map show ns) ++ "]" ++ addParens 1 (show e)
    (Apply e es) -> "Apply" ++ "[" ++ show e ++ ", " ++   intercalate ", " (map show es) ++ "]" 
    (Let d e) -> "Let " ++ show d ++" in " ++ show e
instance Show Action where
  show a = case a of
    (Param n) -> "Var " ++ show n 
    (TermAct t) -> "Term" ++ show t
    (CharAct c) -> ['\'',c,'\'']
    (ListAct as) ->"[" ++ intercalate ", " (map show as) ++ "]"
    (StringAct s) ->"\"" ++ s ++ "\""
    (BoolAct b) -> show b
    (NumAct n) -> show n
    (Closure ns e env) -> "Closure " ++ show ns ++ " " ++ addParens 1 (show e) -- ++ addParens 1 (show env)
    (DefRec n e env) -> "Recursive " ++ show n ++ " " ++ addParens 1 (show e) -- ++ addParens 1 (show env)
    (Application a as) -> "Application" ++ "[" ++ show a ++ ", " ++   intercalate ", " (map show as) ++ "]" 
    (Primitive p) -> show p
