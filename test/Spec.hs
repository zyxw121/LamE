import Test.Hspec
import Control.Exception (evaluate)
import Syntax
import Parser
import Core
import Text.ParserCombinators.Parsec

parseError :: Either a b -> Bool
parseError (Right _) = False
parseError (Left _) = True

main :: IO ()
main = hspec $ do
  describe "pNum" $ do
    it "parses numbers" $ do
      parse pNum "" "4" `shouldBe` Right (NumExp 4)
      parse pNum "" "40" `shouldBe` Right (NumExp 40)
    it "ignores whitespace" $ do
      parse pNum "" "   4" `shouldBe` Right (NumExp 4)
      parse pNum "" " 4 " `shouldBe` Right (NumExp 4)
    it "handles signs" $ do
      parse pNum "" "+4" `shouldBe` Right (NumExp 4)
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

  describe "pList" $ do
    it "errors on empty" $ do
      parse pList "" "" `shouldSatisfy` parseError
    it "works on singles" $ do
      parse pList "" "a" `shouldBe` Right [VarExp $ Name "a"]
      pending "more types of singles"

    


