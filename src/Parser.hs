module Parser where

import AST

import Data.Char
import Text.Parsec ((<|>), (<?>), try, many, many1)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB
import qualified Text.Parsec.Expr as E

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
  t  <- parseType False
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
  ts <- many (try $ parseType True)
  return $ Variant n ts

-- EXPRESSION

{-
parseExp :: Parser Exp
parseExp = (try parseESeqPar) <|> (try parseEAp) <|> parseExpPrim <?> "expression"

parseExpPrim :: Parser Exp
parseExpPrim = bracket "(" parseExp ")"
  <|> (try parseELam)
  <|> (try parseECase)
  <|> (try parseEConst)
  <|> (try parseEPrim)
  <|> (try parseELet)
  <|> parseEVar
-}

parseExp :: Parser Exp
parseExp = E.buildExpressionParser table parseExpPrim
  <?> "expression"

parseExpPrim :: Parser Exp
parseExpPrim = bracket "(" parseExp ")"
  <|> parseELam
  <|> parseECase
  <|> parseEConst
  <|> parseEPrim
  <|> parseELet
  <|> parseEVar
  <?> "simple expression"

table = [ [ E.Infix (return EAp) E.AssocLeft ]
        , [ E.Infix (EPar <$ (string "|")) E.AssocLeft ]
        , [ E.Infix (ESeq <$ (string ";")) E.AssocLeft ]
        ]

parseEVar :: Parser Exp
parseEVar = EVar <$> (try lName)

parseEPrim :: Parser Exp
parseEPrim = EPrim <$> parsePrim

parsePrim :: Parser Prim
parsePrim = PInt <$> integer

parseELet :: Parser Exp
parseELet = do
  n  <- try $ lName <* (string ":")
  mt <- CB.optionMaybe (parseType False)
  string "="
  e  <-parseExpPrim
  return $ ELet n mt e

parseEConst :: Parser Exp
parseEConst = EConst <$> (try uName)

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

parseType :: Bool -> Parser Type
parseType p = (CB.chainr1 (parseTypePrim p) (TFn <$ (string "->"))) <?> "type"

parseTypePrim :: Bool -> Parser Type
parseTypePrim p = 
  (if p then (try $ bracket "(" parseTKind ")") else try parseTKind)
  <|> bracket "(" (parseType False) ")"
  <|> (try parseTPrim)
  <|> parseTGen

parseTPrim :: Parser Type
parseTPrim = TKind <$> uName <*> return []

parseTKind :: Parser Type
parseTKind = do
  n <- uName
  xs <- many ((try parseTPrim) <|> (try $ parseType False))
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

lName :: Parser String
lName = name `satisfy` (\(x:_) -> isLower x)

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



