module Parser.Parser where

import Parser.AST

import Error.Print
import Data.Char
import Text.Parsec ((<|>), (<?>), try, many, many1)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Error as ER

type Parser = P.Parsec String ()

parse :: String -> String -> Either (SourcePos, String) Module
parse file input = 
  case P.runParser parseModule () file input of
    Left err -> Left $ let epos = P.errorPos err 
                       in ( (P.sourceLine epos, P.sourceColumn epos)
                          , showErrorMessages $ ER.errorMessages err)
    Right tl -> Right tl
    
showErrorMessages ms = 
  ER.showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" ms

parseModule :: Parser Module
parseModule = do
  white
  string "mod"
  m  <- uName
  dc <- (many $ (Left <$> parseComb) <|> (Right <$> parseData)) <* P.eof
  return $ Module m dc

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
  ts <- many (try $ parseBType)
  return $ Variant n ts

-- EXPRESSION

parseUExp :: Parser Exp
parseUExp = E.buildExpressionParser table parseExpPrim
  <?> "unit expression"

parseExp :: Parser Exp
parseExp = CB.chainl1 parseUExp (ESeq <$ (string ";"))
  <?> "sequential expression"

parseExpPrim :: Parser Exp
parseExpPrim = bracket "(" parseExp ")"
  <|> parseELam
  <|> parseECase
  <|> ((string ">>") *> (string ";") *> parseExp)
  <|> parseEConst
  <|> parseEPrim
  <|> parseEVar
  <?> "simple expression"

table = [ [ E.Infix (return EAp) E.AssocLeft ]
        , [ E.Infix (EAp <$ (try $ string "$")) E.AssocLeft ]
        , [ E.Prefix parseEIfPrefix ]
        , [ E.Prefix parseELetPrefix ]
        --, [ E.Infix (ESeq <$ (try $ string ";")) E.AssocLeft ]
        ]

parseELetPrefix :: Parser (Exp -> Exp)
parseELetPrefix = do
  n  <- try $ lName <* (string ":")
  mt <- CB.optionMaybe parseType
  string "="
  return $ \ e -> ELet n mt e
  
parseEIfPrefix :: Parser (Exp -> Exp)
parseEIfPrefix = do
  try $ string "if"
  c <- parseExp
  string "then"
  t <- parseExp
  string "else"
  return $ \e -> EIf c t e

parseEVar :: Parser Exp
parseEVar = EVar <$> (try lName)

parseEPrim :: Parser Exp
parseEPrim = EPrim <$> parsePrim

parsePrim :: Parser Prim
parsePrim = PInt <$> integer

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
  vs <- many parseAlter
  return $ ECase e vs

parseAlter :: Parser Alter
parseAlter = do 
  try $ string "|"
  c  <- uName
  vs <- many lName
  string "->" 
  e  <- parseUExp
  return $ (c, vs, e)

-- TYPES

parseType :: Parser Type
parseType = E.buildExpressionParser 
  [ [ E.Infix (return TKind) E.AssocLeft ]
  , [ E.Infix (TFn <$ (string "->")) E.AssocRight ]
  ] ((try parseTPrim) <|> (bracket "(" parseType ")") <|> parseTGen)
  <?> "type"

parseTPrim :: Parser Type
parseTPrim = TPrim <$> uName

parseTGen :: Parser Type
parseTGen = TGen <$> lName

parseBType :: Parser Type
parseBType = bracket "(" parseType ")"
  <|> (try parseTPrim)
  <|> parseTGen
  <?> "btype"

-- PRIMITIVE

parsePrimInt :: Parser Prim
parsePrimInt = PInt <$> integer

-- LEXER

reserved = [ "data", "let", "match", "end", "if", "then", "else"]

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

comment :: Parser ()
comment = (try $ string "/*") >> skip >> return ()
  where skip = (string "*/") <|> (C.anyChar  >> skip)

white :: Parser ()
white = P.skipMany (comment <|> (C.space <|> C.newline <|> C.crlf <|> C.tab) *> return ())

white1 :: Parser ()
white1 = CB.skipMany1 (comment <|> (C.space <|> C.newline <|> C.crlf <|> C.tab) *> return ()) <|> P.eof

