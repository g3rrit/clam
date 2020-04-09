module Parser.Parser where

import Parser.AST

import Util
import Error.Error
import Data.Char
import Text.Parsec ((<|>), (<?>), try, many, many1, sepBy)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as CB
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Error as ER

type Parser = P.Parsec String ()

parse :: File -> String -> EitherError Module
parse file input =
  case P.runParser (parseModule file) () (toPath file) input of
    Left err -> Left $ let epos = P.errorPos err
                           spos = (P.sourceLine epos, P.sourceColumn epos)
                           sloc = Loc spos spos
                       in Error ParserError file
                            (showErrorMessages $ ER.errorMessages err) sloc
    Right tl -> Right tl

showErrorMessages =
  ER.showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input" 

parseModule :: File -> Parser Module
parseModule f = do
  white
  string "mod"
  m  <- uName
  dc <- (many $ (Left <$> parseComb)
          <|> (Right <$> parseData))
          <* P.eof
  return $ Module f m dc

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

-- COMB

parseComb :: Parser Comb
parseComb = tagLoc $ do
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
parseData = (SData <$> parseSumData)
          <|> (PData <$> parseProData)
          <?> "data definition"

parseSumData :: Parser SumData
parseSumData = tagLoc $ do
  try $ string "data"
  n  <- uName
  string "="
  pd <- parseProDataEnv
  pds <- many $ string "|" *> parseProDataEnv
  return $ SumData n $ map (\(pn, pf, pl) -> ProData pn pf pl) (pd:pds)

parseProData :: Parser ProData
parseProData = tagLoc $ do
  try $ string "struct"
  n <- uName 
  string "="
  fs <- parseMembers
  return $ ProData n fs

parseProDataEnv :: Parser (Name, [Member], Loc)
parseProDataEnv = do
  l0 <- getLoc
  n <- uName
  fs <- parseMembers
  l1 <- getLoc
  return $ (n, fs, l0 <> l1)

parseMembers :: Parser [Member]
parseMembers = do
  f <- CB.optionMaybe $ try (parseMemberB 0 <|> parseMemberP 0)
  case f of 
    Nothing -> return []
    Just f' -> (f':) <$> (manyWith $ \c -> (try $ parseMemberS c) <|> (try $ parseMemberB c) <|> parseMemberC c)

parseMemberS :: Integer -> Parser Member
parseMemberS c = string ";" >> ((parseMemberB c) <|> (parseMemberP c))

parseMemberB :: Integer -> Parser Member
parseMemberB c = bracket "(" (parseMemberP c) ")"

parseMemberC :: Integer -> Parser Member
parseMemberC c = tagLoc $ Member (Right c) <$> parseType

parseMemberP :: Integer -> Parser Member
parseMemberP c =
  ( try $ tagLoc
      ( do 
          n <- lName
          string ":"
          t <- parseType
          return $ Member (Left n) t
      )
  ) <|> (parseMemberC c)
  
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
  <|> parseEVar
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

parseEVar :: Parser Exp
parseEVar = tagLoc $ EVar <$> (try name)

parseEPrim :: Parser Exp
parseEPrim = tagLoc $ EPrim <$> parsePrim

parsePrim :: Parser Prim
parsePrim = (PInt <$> integer)

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
  [ 
  [ E.Infix (TFn <$ (string "->")) E.AssocRight ]
  ] parseTType
  <?> "type"

parseTPrim :: Parser Type
parseTPrim = tagLoc $ TPrim <$> uName

parseBType = E.buildExpressionParser
  [ 
  ] parseTType
  <?> "btype"

parseTType :: Parser Type
parseTType = bracket "(" parseType ")"
  <|> (try $ parseTPrim)
  <?> "ttype"

-- PRIMITIVE

parsePrimInt :: Parser Prim
parsePrimInt = PInt <$> integer

-- UTIL

manyWith :: (Integer -> Parser a) -> Parser [a]
manyWith f = manyWith' 1
  where 
    manyWith' c = 
      (try $ manyWith1' c) <|> return []
    manyWith1' c = 
      (:) <$> (f c) <*> (manyWith' $ c + 1)

-- LEXER

reserved = [ "data", "struct", "let", "match", "end", "if", "then", "else"]

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
