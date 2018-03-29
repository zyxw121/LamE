{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Syntax
import Core
import Data.Char
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


rnames = words "true false if then else let val rec func in = "
rops = words "+ - * / < > and or not =" --get rid of these maybe?

languageDef = 
  emptyDef  { Token.commentLine     = "//"
            , Token.identStart      = letter <|> oneOf "+-*/<=>"
            , Token.identLetter     = oneOf alloweds
            , Token.reservedNames   = rnames
            , Token.reservedOpNames = rops
            , Token.caseSensitive = False 
            }

lexer = Token.makeTokenParser languageDef

--identifier = Token.identifier lexer >>= return . Name-- parses an identifier as a Name
--reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
--parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
semi       = Token.semi       lexer

--Helper funcs
parens :: Parser a -> Parser a
parens p = do
  char '('
  x <- p
  char ')'
  return x

white :: Parser ()
white = skipMany1 space

ident :: Parser String
ident = do
  x <- letter <|> oneOf "+-*/<=>"
  xs <- many $ oneOf alloweds 
  return (x:xs)

identifier :: Parser Name
identifier = try $ do
  name <- ident
  if name `elem` rnames then unexpected ("reserved word") else return . Name $ name

reserved :: String -> Parser ()
reserved name = try $ do 
  _ <- caseString name
  notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)

caseString :: String -> Parser String
caseString name = do{ walk name; return name }
  where
    walk []     = return ()
    walk (c:cs) = do{ _ <- caseChar c <?> msg; walk cs }
    caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                | otherwise  = char c
    msg         = show name 


-- parsing Exprs
pExpr :: Parser Expr
pExpr = do
  skipMany space
  xs <- sepEndBy1 pExpr' white
  case xs of
    [] -> unexpected "empty"
    [x] -> return x
    (x:y:ys) -> return $ Apply x (y:ys)

pExpr' :: Parser Expr
pExpr' = pConst <|> parens pExpr

pConst = pVar <|> pNum <|> pBool

pNum :: Parser Expr
pNum = integer >>= return . NumExp . fromIntegral

pBool :: Parser Expr
pBool = (reserved "true" >> return (BoolExp True))
     <|>(reserved "false" >> return (BoolExp False))

pVar :: Parser Expr
pVar = identifier >>= return . VarExp 

parseExpr :: String -> Expr
parseExpr s = case parse (pExpr <* eof) "" s of
  Left e -> error $ show e
  Right r -> r

pDefn :: Parser Defn
pDefn = undefined

pProgram :: Parser (Program )
pProgram = do
  defs <- many (pDefn >>= (\x -> semi >> return x)) --list of ';' seperated defns
  e <- pExpr
  return $ Program defs e

parseStrP :: String -> Program
parseStrP s = case parse pProgram "" s of
  Left e -> error $ show e
  Right r -> r
