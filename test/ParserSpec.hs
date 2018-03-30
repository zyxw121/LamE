module ParserSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Syntax
import Parser
import Core
import Test.QuickCheck
import Text.ParserCombinators.Parsec

parseError :: Either a b -> Bool
parseError (Right _) = False
parseError (Left _) = True

var :: String -> Expr
var = VarExp . Name


spec :: Spec
spec = do
  describe "pNum" $ do
    it "parses numbers" $ property $
      \x -> parse pNum "" (show x) == Right (NumExp x) 
    it "ignores leading zeroes" $ do
      parse pNum "" "0" `shouldBe` Right (NumExp 0)
      parse pNum "" "0004" `shouldBe` Right (NumExp 4)
      parse pNum "" "000" `shouldBe` Right (NumExp 0)
--    it "ignores whitespace" $ do
--      parse pNum "" "   4" `shouldBe` Right (NumExp 4)
--      parse pNum "" " 4 " `shouldBe` Right (NumExp 4)
    it "handles signs" $ do
      parse pNum "" "-4 " `shouldBe` Right (NumExp (-4))
  describe "pBool" $ do
    it "parses correctly" $ do
      parse pBool "" "true" `shouldBe` Right (BoolExp True)
      parse pBool "" "false" `shouldBe` Right (BoolExp False)
    it "ignores whitespace" $ do
      parse pBool "" "false    " `shouldBe` Right (BoolExp False)
    it "ignores cases" $ do
      parse pBool "" "FALSE" `shouldBe` Right (BoolExp False)
      parse pBool "" "FaLsE" `shouldBe` Right (BoolExp False)
  describe "pChar" $ do
    it "parses any char" $ property $
      \x -> parse pChar "" ("\'"++[x]++"\'") == Right (CharExp x)
    it "won't parse empty or multiple" $ do
      parse pChar "" "\'\'" `shouldSatisfy` parseError
      parse pChar "" "\'aa\'" `shouldSatisfy` parseError
-- Strings are annoying, but this probably works
--  describe "pString" $ do
--    it "parses any string" $ property $ 
--      \x -> parse pString "" (x) == Right (StringExp x)

  describe "pList" $ do
    it "parses empty" $ do
      parse pList "" "[]" `shouldBe` Right (ListExp [])
      parse pList "" "[    ]" `shouldBe` Right (ListExp [])
    it "parses lists" $ property $ 
      parse pList "" "[ 1   , 3,-1,5, 4  , -5  ]" `shouldBe` Right ( ListExp (map NumExp [1,3,-1,5,4,-5]))
  describe "pTerm" $ do
    it "parses vars" $ do
      parse pExpr "" "(Var \"a\")" `shouldBe` Right (TermExp (Var (Name "a")))
    it "parses App" $ do
      parse pExpr "" "(App (Var \"a\") (Var \"b\") )" `shouldBe` Right (TermExp (App (Var (Name "a")) (Var (Name "b"))))
    it "parses Abs" $ do
      parse pExpr "" "(Abs \"a\" (Var \"a\") )" `shouldBe` Right (TermExp (Abs (Name "a") (Var (Name "a")) ))
  describe "pVar" $ do
    it "parses letters" $ do
      parse pVar "" "a" `shouldBe` Right (VarExp $ Name "a")
      parse pVar "" "abc" `shouldBe` Right (VarExp $ Name "abc")
      parse pVar "" "ABC" `shouldBe` Right (VarExp $ Name "ABC")
    it "parses good ops" $ do
      parse pVar "" "+" `shouldBe` Right (VarExp $ Name "+")
      parse pVar "" "-" `shouldBe` Right (VarExp $ Name "-")
      parse pVar "" "*" `shouldBe` Right (VarExp $ Name "*")
      parse pVar "" "/" `shouldBe` Right (VarExp $ Name "/")
      parse pVar "" "==" `shouldBe` Right (VarExp $ Name "==")
      parse pVar "" "<" `shouldBe` Right (VarExp $ Name "<")
      parse pVar "" ">" `shouldBe` Right (VarExp $ Name ">")
      parse pVar "" "<=" `shouldBe` Right (VarExp $ Name "<=")
      parse pVar "" ">=" `shouldBe` Right (VarExp $ Name ">=")
    it "but not bad singles" $ do
      parse pVar "" "1" `shouldSatisfy` parseError 
      parse pVar "" "=" `shouldSatisfy` parseError 
      parse pVar "" "@" `shouldSatisfy` parseError 
    it "obeys the rules " $ do
      parse pVar "" "a" `shouldBe` Right (VarExp $ Name "a")
      parse pVar "" "a1" `shouldBe` Right (VarExp $ Name "a1")
--      parse pVar "" "a!b" `shouldSatisfy` parseError
    it "ignores whitespace" $ do
      parse pVar "" "abc   " `shouldBe` Right (VarExp $ Name "abc")
    it "doesn't parse reserveds" $ do
      parse pVar "" "true" `shouldSatisfy` parseError 
      parse pVar "" "false" `shouldSatisfy` parseError 
      parse pVar "" "if" `shouldSatisfy` parseError 
      parse pVar "" "then" `shouldSatisfy` parseError 
      parse pVar "" "else" `shouldSatisfy` parseError 
      parse pVar "" "let" `shouldSatisfy` parseError 
      parse pVar "" "val" `shouldSatisfy` parseError 
      parse pVar "" "rec" `shouldSatisfy` parseError 
      parse pVar "" "func" `shouldSatisfy` parseError 
      parse pVar "" "in" `shouldSatisfy` parseError 
      parse pVar "" "=" `shouldSatisfy` parseError 
    it "but parses things containing them" $ do
      parse pVar "" "trues" `shouldBe` Right (VarExp $ Name "trues")
      parse pVar "" "ffalse" `shouldBe` Right (VarExp $ Name "ffalse")
      parse pVar "" "tiffany" `shouldBe` Right (VarExp $ Name "tiffany")
  describe "pIf" $ do
    it "ignores whitespace" $ do
      parse pIf "" "if   x   then   y   else   z  " `shouldBe` Right (If (var "x") (var "y") (var "z") )
  describe "pArgs" $ do
    it "accepts singles" $ do
      parse pArgs "" "(x)" `shouldBe` Right ([Name "x"])
      parse pArgs "" "( x )" `shouldBe` Right ([Name "x"])
    it "and multiples" $ do
      parse pArgs "" "(x y)" `shouldBe` Right ([Name "x", Name "y"])
      parse pArgs "" "( x   y   )" `shouldBe` Right ([Name "x", Name "y"])
    it "but not empty" $ do
      parse pArgs "" "()" `shouldSatisfy` parseError
      parse pArgs "" "(    )" `shouldSatisfy` parseError
  describe "pFunc" $ do
    it "ignores whitespace" $ do
      parse pFunc "" "func (x) (x)" `shouldBe` Right (Func [Name "x"] (var "x"))
      parse pFunc "" "func (  x  )   (  x  )" `shouldBe` Right (Func [Name "x"] (var "x"))
      parse pFunc "" "func(x)(x)" `shouldBe` Right (Func [Name "x"] (var "x"))
    it "requires parens" $ do
      parse pFunc "" "func x) (x)" `shouldSatisfy` parseError
      parse pFunc "" "func (x y) x" `shouldSatisfy` parseError
  describe "pLet" $ do
    it "ignores whitespace" $ do
      parse pLet "" "let   val   x   =   1  in   x" `shouldBe` Right ( Let (Val (Name "x") (NumExp 1)) (var "x"))
  describe "pVal" $ do
    it "requires whitespace around the =" $ do
      parse pVal "" "val x = y" `shouldBe` Right (Val (Name "x") (var "y"))
      parse pVal "" "val x =y" `shouldSatisfy` parseError
      parse pVal "" "val x= y" `shouldSatisfy` parseError
    it "allows whitespace" $ do
      parse pVal "" "val   x   =   y" `shouldBe` Right (Val (Name "x") (var "y"))
  describe "pRec" $ do
    it "requires whitespace around the =" $ do
      parse pRec "" "rec x = y" `shouldBe` Right (Rec (Name "x") (var "y"))
      parse pRec "" "rec x =y" `shouldSatisfy` parseError
      parse pRec "" "rec x= y" `shouldSatisfy` parseError
    it "allows whitespace" $ do
      parse pRec "" "rec   x   =   y" `shouldBe` Right (Rec (Name "x") (var "y"))
  describe "pExpr" $ do
    it "allows parens" $ do
      parse pExpr "" "x" `shouldBe` Right (var "x")
      parse pExpr "" "((x))" `shouldBe` Right (var "x")
    it "applies right" $ do
      parse pExpr "" "f x" `shouldBe` Right (Apply (var "f") [var "x"])
      parse pExpr "" "f x (g y)" `shouldBe` Right (Apply (var "f") [var "x", Apply (var "g") [var "y"]])
      parse pExpr "" "f x (g y) z" `shouldBe` Right (Apply (var "f") [var "x", Apply (var "g") [var "y"], var"z"])
  describe "pProgram" $ do
    it "" $ do
      pending 
