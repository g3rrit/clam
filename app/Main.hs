module Main where

import Parser

version = "0.0.0"

main :: IO ()
main = do
  putStrLn $ "CLAM " ++ version
  file <- readFile "test/example.cl"
  print file
  print $ parse "TEST" file
