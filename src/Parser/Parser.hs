module Parser.Parser where

import Parser.AST

import Error.Print
import Data.Char
import Text.Parsec ((<|>), (<?>), try, many, many1, sepBy)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Error as ER

type Parser = P.Parsec String ()

data PError = PError Loc String

parse :: String -> String -> Either PError Module
parse file input =
  case P.runParser parseModule () file input of
    Left err -> Left $ let epos = P.errorPos err
                           spos = (P.sourceLine epos, P.sourceColumn epos)
                           sloc = Loc spos spos
                       in PError sloc
                            $ showErrorMessages $ ER.errorMessages err
    Right tl -> Right tl

showErrorMessages ms =
  ER.showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" ms

parseModule :: Parser Module
parseModule = do
  white
  string "mod"
  m  <- uName
  dc <- (many $ (Left <$> (tagLoc parseComb))
          <|> (Right <$> (tagLoc parseData)))
          <* P.eof
  return $ Module m dc

-- UTIL

getLoc :: Parser Loc
getLoc = do
  p <- P.getPosition
  let sp = (P.sourceLine p, P.sourceColumn p)
  return $ Loc sp sp

tagLoc :: Parser (Loc -> a) -> Parser a
tagLoc p = do
  l0 <- getLoc
  r  <- p
  l1 <- getLoc
  return $ r (l0 <> l1)

-- TEMPLATE

parseTemplate :: Parser Template
parseTemplate = do
  ts <- bracket "<" (many lName) ">"
  return $ Template ts

-- COMB

parseComb :: Parser (Loc -> Comb)
parseComb = do
  try $ string "let"
  n  <- lName
  tp <- CB.optionMaybe parseTemplate
  vs <- many lName
  string ":"
  t  <- parseType
  string "="
  e  <- parseExp
  return $ Comb n tp vs t e

-- DATA

parseData :: Parser (Loc -> Data)
parseData = do
  try $ string "data"
  n  <- uName
  tp <- CB.optionMaybe parseTemplate
  ns <- many lName
  string "="
  v  <- parseVariant
  vs <- many $ string "|" *> parseVariant
  return $ Data n tp ns (v:vs)

parseVariant :: Parser Variant
parseVariant = tagLoc $ do
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
  <|> parseELet
  <|> parseEIf
  <|> parseELam
  <|> parseECase
  <|> ((string ">>") *> (string ";") *> parseExp)
  <|> parseEPrim
  <?> "simple expression"

table = [ [ E.Infix (return EAp) E.AssocLeft ]
        , [ E.Infix (EAp <$ (try $ string "$")) E.AssocLeft ]
        ]

parseELet :: Parser Exp
parseELet = tagLoc $ do
  n  <- try $ lName <* (string ":")
  mt <- CB.optionMaybe parseType
  string "="
  e  <- parseUExp
  return $ ELet n mt e

parseEIf :: Parser Exp
parseEIf = tagLoc $ do
  try $ try $ string "if"
  c <- parseExp
  string "then"
  t <- parseExp
  string "else"
  e <- parseUExp
  return $ EIf c t e

parseEPrim :: Parser Exp
parseEPrim = tagLoc $ EPrim <$> parsePrim

parsePrim :: Parser Prim
parsePrim = (PInt <$> integer)
          <|> (PVar <$> name)

parseELam :: Parser Exp
parseELam = tagLoc $ do
  try $ string "\\"
  ns <- many lName
  string "->"
  e  <- parseUExp
  return $ ELam ns e

parseECase :: Parser Exp
parseECase = tagLoc $ do
  try $ string "match"
  e <- parseExp
  vs <- many parseAlter
  return $ ECase e vs

parseAlter :: Parser Alter
parseAlter = tagLoc $ do
  try $ string "|"
  c  <- uName
  vs <- many lName
  string "->"
  e  <- parseUExp
  return $ Alter c vs e

-- TYPES

parseType :: Parser Type
parseType = E.buildExpressionParser
  [ [ E.Prefix (TRef <$ (string "&")) ]
  , [ E.Prefix (TSptr <$ (string "*")) ]
  , [ E.Prefix (TUptr <$ (string "^")) ]
  , [ E.Infix (return TKind) E.AssocLeft ]
  , [ E.Infix (TFn <$ (string "->")) E.AssocRight ]
  ] parseTType
  <?> "type"

parseTPrim :: Parser Type
parseTPrim = tagLoc $ TPrim <$> uName

parseTGen :: Parser Type
parseTGen = tagLoc $ TGen <$> lName

parseBType = E.buildExpressionParser
  [ [ E.Prefix (TRef <$ (string "&")) ]
  , [ E.Prefix (TSptr <$ (string "*")) ]
  , [ E.Prefix (TUptr <$ (string "^")) ]
  ] parseTType
  <?> "btype"

parseTType :: Parser Type
parseTType = bracket "(" parseType ")"
  <|> (try $ parseTPrim)
  <|> parseTGen
  <?> "ttype"

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
  where skip = (try $ string "*/") <|> (C.anyChar  >> skip)

white :: Parser ()
white = P.skipMany (comment <|> (C.space <|> C.newline <|> C.crlf <|> C.tab) *> return ())

white1 :: Parser ()
white1 = CB.skipMany1 (comment <|> (C.space <|> C.newline <|> C.crlf <|> C.tab) *> return ()) <|> P.eof
