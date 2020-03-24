module Main where

import Util
import Error.Print
import Text.PrettyPrint
import Parser.Parser
import Parser.Pretty
import Pipe

getConfig :: IO Config
getConfig = return $ Config 
  { cversion = "0.0.0"
  , cpath    = "test"
  , cbackend = BackendCpp
  , coutput  = "./a.out"
  , ccc      = "clang++"
  }

main :: IO ()
main = do
  c <- getConfig
  putStrLn $ "CLAM " ++ (cversion c)
  run c $ pipe ["test/example.cl"]
  putStrLn "Done!"
