module Main where

import Error.Print
import Text.PrettyPrint
import Parser.Parser
import Parser.Pretty

version = "0.0.0"

main :: IO ()
main = do
  putStrLn $ "CLAM " ++ version
  file <- readFile "test/example.cl"
  print file
  let r = parse "TEST" file
  case r of
    Left (p, e) -> printError "test/example.cl" p ParserError e
    Right ast -> putStrLn $ render $ pretty ast
