module Parser where
import Syntax
import Core
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


rnames = words "true false if then else let val rec func in "
rops = words "+ - * / < > and or not =" --get rid of these maybe?

languageDef = 
  emptyDef  { Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = oneOf alloweds
            , Token.reservedNames   = rnames
            , Token.reservedOpNames = rops
            
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer >>= return . Name-- parses an identifier as a Name
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace
semi       = Token.semi       lexer

white :: Parser ()
white = do
  s <- many1 space
  return ()

pExpr :: Parser (Expr )
pExpr = pVar
      <|> pNum 
      <|> pIf 
      <|> try (parens (pFunc <|> pApply <|> pLet)) 
      <|> parens pExpr

pNum :: Parser (Expr )
pNum = integer >>= return . NumExp

pVar :: Parser (Expr )
pVar = identifier >>= return . VarExp

pIf :: Parser (Expr )
pIf = do
  reserved "if"
  cond <- pExpr
  reserved "then"
  left <- pExpr
  reserved "else"
  right <- pExpr
  return $ If cond left right

pFunc :: Parser (Expr )
pFunc = do
  reserved "func"
  xs <- parens $ sepBy1 identifier white
  body <- parens $ pExpr
  return $ Func xs body

pApply :: Parser (Expr )
pApply = do
  f <- pExpr
  args <- sepBy1 pExpr whiteSpace
  return $ Apply f args

pLet :: Parser (Expr )
pLet = do
  reserved "let"
  def <- pDefn
  reserved "in"
  e <- pExpr
  return $ Let def e

pDefn :: Parser (Defn )
pDefn = pVal <|> pRec

pVal :: Parser (Defn )
pVal = do
  reserved "val"
  x <- identifier
  reservedOp "="
  e <- pExpr
  return $ Val x e

pRec :: Parser (Defn )
pRec = do
  reserved "rec"
  x <- identifier
  reservedOp "="
  e <- pExpr
  return $ Rec x e

pProgram :: Parser (Program )
pProgram = do
  defs <- many (pDefn >>= (\x -> semi >> return x)) --list of ';' seperated defns
  e <- pExpr
  return $ Program defs e

