{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Syntax
import Core
import Data.Char
import Text.ParserCombinators.Parsec hiding (string) 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


reserveds = words "true false if then else let val rec func in = match as"
rops = words "+ - * / < > and or not =" --get rid of these maybe?

semi :: Parser ()
semi = do
  char ';'
  skipMany white

--Helper funcs
zeroNumber :: Parser Int
zeroNumber  = do
  _ <- char '0'
  number  <|> return 0

number = do
  digits <- many1 digit
  let n = foldl (\x d -> 10*x +  (digitToInt d)) 0 digits
  seq n (return n)

integer :: Parser Int
integer = do
  f <- option id (char '-' >> return negate)
  n <- zeroNumber <|> number
  return $ f n 

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

identifier' :: Parser Name
identifier' = do
  x <- identifier
  skipMany space
  return x

identifier :: Parser Name
identifier = try $ do
  name <- ident
  if name `elem` reserveds then unexpected ("reserved word") else return . Name $ name

reserved :: String -> Parser ()
reserved s = reserved' s >> skipMany space

reserved' :: String -> Parser ()
reserved' name = try $ do 
  _ <- caseString name
  notFollowedBy (oneOf alloweds) <?> ("end of " ++ show name)

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
pExpr' = pConst 
      <|> pIf <|> pLet <|> pFunc <|> pMatch
      <|> parens pExpr

-- Constants
pConst =  try pNum <|> pVar <|> pBool <|> pChar <|> pString <|> pList

pTerm :: Parser Term
pTerm = (pVarT <|> pApp <|> pAbs)

pVarT = do
  reserved "Var"
  x <- string
  return . Var . Name $ x

pApp = do
  reserved "App"
  s <- parens pTerm
  white
  t <- parens pTerm 
  return $ App s t

pAbs = do
  reserved "Abs"
  x <- string
  white
  s <- parens pTerm
  return $ Abs (Name x) s

pNum :: Parser Expr
pNum = integer >>= return . NumExp 

pBool :: Parser Expr
pBool = (reserved' "true" >> return (BoolExp True))
     <|>(reserved' "false" >> return (BoolExp False))

pChar :: Parser Expr
pChar = do
  char '\''
  x <- anyChar
  char '\''
  return $ CharExp x

pEscape :: Parser Char
pEscape = do 
  char '\\' 
  x <- oneOf "\\\"nrt" 
  return $ case x of 
    '\\' -> x
    '"'  -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

comma :: Parser ()
comma = do
  skipMany space
  char ','
  skipMany space

pList :: Parser Expr
pList = do
  char '['
  skipMany space
  xs <- sepEndBy pExpr comma <|> sepBy pExpr comma
  skipMany space
  char ']'
  return $ ListExp xs

string :: Parser String
string = do
  char '\"'
  xs <- many $ pEscape <|> noneOf "\"\\"
  char '\"'
  return xs

pString :: Parser Expr
pString = string >>= return .  StringExp 

pVar :: Parser Expr
pVar = identifier >>= return . VarExp 

-- Others
pIf :: Parser Expr
pIf = do
  reserved "if"
  c <- pExpr
  reserved "then"
  x <- pExpr
  reserved "else"
  y <- pExpr
  return $ If c x y

-- ewww
pMatch :: Parser Expr
pMatch = do
  reserved "match"
  x <- pExpr
  reserved "as"
  char '('
  skipMany space
  reserved "Var"
  x1 <- identifier'
  char ')'
  skipMany space
  e1 <- parens pExpr
  skipMany space
  char '('
  reserved "App"
  s2 <- identifier'
  t2 <- identifier'
  char ')'
  skipMany space
  e2 <- parens pExpr
  skipMany space
  char '('
  reserved "Abs"
  x3 <- identifier'
  s3 <- identifier'
  char ')'
  skipMany space
  e3 <- parens pExpr  
  return $ Match x (x1, e1) (s2,t2,e2) (x3,s3,e3)


pArgs :: Parser [Name]
pArgs = do
  char '('
  skipMany space
  xs <- sepEndBy1 identifier white <|> sepBy1 identifier white
  skipMany space
  char ')'
  return xs

pFunc :: Parser Expr
pFunc = do
  reserved "func"
  xs <- pArgs 
  skipMany space
  body <- parens pExpr
  return $ Func xs body

pLet :: Parser Expr
pLet = do
  reserved "let"
  d <- pDefn
  reserved "in"
  e <- pExpr
  return $ Let d e
  
parseExpr :: String -> Expr
parseExpr s = case parse (pExpr <* eof) "" s of
  Left e -> error $ show e
  Right r -> r

pDefn :: Parser Defn
pDefn = pVal <|> pRec

pVal :: Parser Defn
pVal = do
  reserved "val"
  x <- try identifier'
  reserved "="
  e <- pExpr
  return $ Val x e 

pRec :: Parser Defn
pRec = do
  reserved "rec"
  x <- identifier'
  reserved "="
  e <- pExpr
  return $ Rec x e 

pProgram :: Parser Program
pProgram = do
  defs <- many (pDefn >>= (\x -> semi >> return x)) --list of ';' seperated defns
  e <- pExpr
  return $ Program defs e

parseStrP :: String -> Program
parseStrP s = case parse pProgram "" s of
  Left e -> error $ show e
  Right r -> r
