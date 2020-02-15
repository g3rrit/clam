module Main where

import Text.PrettyPrint
import Parser
import Pretty

version = "0.0.0"

main :: IO ()
main = do
  putStrLn $ "CLAM " ++ version
  file <- readFile "test/example.cl"
  print file
  let r = parse "TEST" file
  putStrLn $ case r of
    Left e -> e
    Right ast -> render $ pretty ast
