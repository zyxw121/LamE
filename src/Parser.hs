{-# LANGUAGE FlexibleContexts #-}
module Parser where
import Syntax
import Core
import Data.Char
import Text.ParserCombinators.Parsec hiding (spaces) 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


alloweds = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-+*/<>=%"
reserveds = words "true false if then else let val rec func in = match as"

--Helper functions
semi :: Parser ()
semi = do
  char ';'
  skipMany spaces1

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
parens = enclosed '(' ')'

enclosed :: Char -> Char -> Parser a -> Parser a
enclosed l r p = do
  char l
  x <- p
  char r
  return x

spaces :: Parser ()
spaces = skipMany space
spaces1 :: Parser ()
spaces1 = skipMany1 space

ident :: Parser String
ident = do
  x <- letter <|> oneOf "+-*/<=>%"
  xs <- many $ oneOf alloweds 
  return (x:xs)

identifier :: Parser Name
identifier = try $ do
  name <- ident
  if name `elem` reserveds then unexpected ("reserved word") else return . Name $ name

identifier' :: Parser Name
identifier' = eat identifier 

eat :: Parser a -> Parser a
eat p = do
  x <- p
  spaces
  return x

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
  xs <- sepEndBy1 pExpr' spaces1
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
pList =  enclosed '[' ']' $ do
  skipMany space
  xs <- eat $ sepEndBy pExpr comma <|> sepBy pExpr comma
  return $ ListExp xs

string' :: Parser String
string' = enclosed '\"' '\"' $
  many $ pEscape <|> noneOf "\"\\"

pString :: Parser Expr
pString = string' >>= return . StringExp 

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

pMatch :: Parser Expr
pMatch = do
  reserved "match"
  x <- pExpr
  reserved "as"
  x1 <- eat $ parens $ spaces >> reserved "Var" >> identifier'
  e1 <- eat $ parens pExpr
  [s2,t2] <- eat $ parens $ reserved "App" >> many identifier'
  e2 <- eat $ parens pExpr
  [x3,s3] <- eat $ parens $ reserved "Abs" >> many identifier'
  e3 <- parens pExpr  
  return $ Match x (x1, e1) (s2,t2,e2) (x3,s3,e3)

pArgs :: Parser [Name]
pArgs = parens $ do 
  skipMany space
  xs <- sepEndBy1 identifier spaces1 <|> sepBy1 identifier spaces1
  skipMany space
  return xs

pFunc :: Parser Expr
pFunc = do
  reserved "func"
  xs <- eat pArgs 
  body <- parens pExpr
  return $ Func xs body

pLet :: Parser Expr
pLet = do
  reserved "let"
  d <- pDefn
  reserved "in"
  e <- pExpr
  return $ Let d e
  
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
  e <- pExpr <* eof
  return $ Program defs e

pCommand :: Parser Command
pCommand = try (pDefn >>= return . Define) 
        <|> try pEval 
        <|> (char ':' >> pStringCommand)

pEval :: Parser Command
pEval = do
  p <- pMode
  case p of
    None -> spaces
    _ -> spaces1
  e <- pExpr
  return $ Evaluate e p 

pMode :: Parser Process 
pMode = try (char ':' >> ((string "bnf" >> return Bnf ) 
      <|> (string "hnf" >> return Hnf)
      <|> (string "int" >> return ToInt)
      <|> (string "bool" >> return ToBool)
      <|> (string "char" >> return ToChar)
      <|> (string "string" >> return ToString))) 
      <|> return None

pStringCommand :: Parser Command
pStringCommand = ((string "q" <|> string "quit") >> return Quit)
              <|> (string "r" >> return Reset)
              <|> (string "l" >> spaces1 >> many anyChar >>= return . Load)
          

-- Parsing strings
parseCom :: String -> Command 
parseCom s = case parse pCommand "" s of
  Left e -> Bad (show e) s 
  Right r -> r

parseProg :: String -> Either String Program
parseProg s = case parse pProgram "" s of
  Left e -> Left $ show e
  Right r -> Right r

parseExpr :: String -> Expr
parseExpr s = case parse (pExpr <* eof) "" s of
  Left e -> error $ show e
  Right r -> r

parseStrP :: String -> Program
parseStrP s = case parse pProgram "" s of
  Left e -> error $ show e
  Right r -> r
