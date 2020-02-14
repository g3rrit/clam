module Parser where

import AST

import Data.Char
import Text.Parsec ((<|>), (<?>), try, many, many1)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB

type Parser = P.Parsec String ()

parse :: String -> String -> Either String Toplevel
parse file input = 
  case P.runParser toplevel () file input of
    Left err -> Left $ show err
    Right tl -> Right tl
    

toplevel :: Parser Toplevel
toplevel = white *> (many $ (Left <$> parseComb) <|> (Right <$> parseData)) <* P.eof

-- COMB

parseComb :: Parser Comb
parseComb = do
  try $ string "let"
  n  <- lName
  vs <- many lName
  string ":"
  t  <- parseType
  string "="
  e  <- parseExp
  return $ Comb n vs t e

-- DATA

parseData :: Parser Data
parseData = do
  try $ string "data"
  n  <- uName
  ns <- many lName 
  string "="
  v  <- parseVariant
  vs <- many $ string "|" *> parseVariant
  return $ Data n ns (v:vs)

parseVariant :: Parser Variant
parseVariant = do
  n  <- uName
  ts <- many (try parseType)
  return $ Variant n ts

-- EXPRESSION

parseExp :: Parser Exp
parseExp = (try parseESeqPar) <|> parseExpPrim <?> "expression"

parseExpPrim :: Parser Exp
parseExpPrim = bracket "(" parseExp ")"
  <|> (try parseELam)
  <|> (try parseECase)
  <|> (try parseEConst)
  <|> (try parseEPrim)
  <|> (try parseELet)
  <|> parseEVar

parseEVar :: Parser Exp
parseEVar = EVar <$> lName

parseEPrim :: Parser Exp
parseEPrim = EPrim <$> parsePrim

parsePrim :: Parser Prim
parsePrim = PInt <$> integer

parseESeqPar :: Parser Exp
parseESeqPar = CB.chainl1 parseExpPrim ((EPar <$ (string "|")) <|> (ESeq <$ (string ";")))

parseELet :: Parser Exp
parseELet = do
  n  <- lName
  string ":"
  mt <- CB.optionMaybe parseType
  string "="
  e  <-parseExpPrim
  return $ ELet n mt e

parseEConst :: Parser Exp
parseEConst = EConst <$> uName

parseEAp :: Parser Exp
parseEAp = CB.chainl1 parseExpPrim $ return EAp

parseELam :: Parser Exp
parseELam = do
  try $ string "["
  ns <- many lName
  string "->"
  e  <- parseExp 
  string "]"
  return $ ELam ns e

parseECase :: Parser Exp
parseECase = do
  try $ string "match"
  e <- parseExp 
  string "{"
  vs <- many parseAlter
  string "}"
  return $ ECase e vs

parseAlter :: Parser Alter
parseAlter = do 
  try $ string ">"
  c  <- uName
  vs <- many lName
  string "->" 
  e  <- parseExp
  return $ (c, vs, e)

-- TYPES

parseType :: Parser Type
parseType = (try parseTFn) <|> parseTypePrim <?> "type"

parseTypePrim :: Parser Type
parseTypePrim = (try $ bracket "(" parseTKind ")")
  <|> bracket "(" parseType ")"
  <|> (try parseTPrim)
  <|> parseTGen

parseTFn :: Parser Type
parseTFn = CB.chainr1 parseTypePrim (TFn <$ (string "->"))

parseTPrim :: Parser Type
parseTPrim = TKind <$> uName <*> return []

parseTKind :: Parser Type
parseTKind = do
  n <- uName
  xs <- many ((try parseTPrim) <|> (try parseType))
  return $ TKind n xs

parseTGen :: Parser Type
parseTGen = TGen <$> lName

-- PRIMITIVE

parsePrimInt :: Parser Prim
parsePrimInt = PInt <$> integer

-- LEXER

reserved = [ "data", "let", "match"]

satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p f = do 
  r <- p
  if f r then return r else P.parserZero

bracket :: String -> Parser a -> String -> Parser a
bracket l p r = (try $ string l) *> p <* (string r)

lower :: Parser Char
lower = C.lower <* white1

uName :: Parser String
uName = name `satisfy` (\(x:_) -> isUpper x)

-- ((:) <$> C.upper <*> (many C.alphaNum)) <* white

lName :: Parser String
lName = name `satisfy` (\(x:_) -> isLower x)

--((:) <$> C.lower <*> (many C.alphaNum)) <* white

name :: Parser String
name = ((:) <$> C.letter <*> (many C.alphaNum)) `satisfy` (\n -> not $ elem n reserved) <* white

string :: String -> Parser String
string n = C.string n <* white

sstring :: Parser String
sstring = (try $ C.char '\"') *> (many C.anyChar) <* (C.char '\"') <* white

char :: Parser Char
char = (try $ C.char '\'') *> C.anyChar <* (C.char '\'') <* white

integer :: Parser Int
integer = (rd <$> many1 C.digit) <* white
  where rd = read :: String -> Int

white :: Parser ()
white = P.skipMany (C.space <|> C.newline <|> C.crlf <|> C.tab)

white1 :: Parser ()
white1 = CB.skipMany1 (C.space <|> C.newline <|> C.crlf <|> C.tab) <|> P.eof



