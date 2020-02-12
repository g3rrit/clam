module Parser where

import AST

import Text.Parsec ((<|>), try, many, many1)
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
toplevel = do
  undefined

-- DATA

parseData :: Parser Data
parseData = 
  undefined

parseConst :: Parser Const
parseConst = do
  n  <- name
  ts <- many parseType
  return $ Const n ts

-- TYPES

parseType :: Parser Type
parseType = do
  undefined 

-- PRIMITIVE

parsePrimInt :: Parser Prim
parsePrimInt = undefined

-- LEXER

name :: Parser String
name = white *> ((:) <$> C.letter <*> (many1 C.alphaNum))

string :: String -> Parser String
string n = white *> C.string n

sstring :: Parser String
sstring = white *> (try $ C.char '\"') *> (many C.anyChar) <* (C.char '\"')

char :: Parser Char
char = white *> (try $ C.char '\'') *> C.anyChar <* (C.char '\'')

integer :: Parser Int
integer = white *> (rd <$> many1 C.digit)
  where rd = read :: String -> Int

white :: Parser ()
white = 
  P.skipMany (C.space <|> C.newline <|> C.crlf <|> C.tab)


