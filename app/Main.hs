module Main where

import Error.Print
import Text.PrettyPrint
import Parser.Parser
import Parser.Pretty
import Pipe

version = "0.0.0"

main :: IO ()
main = do
  putStrLn $ "CLAM " ++ version
  run ["test/example.cl"] "cpp"
  putStrLn "Done!"
