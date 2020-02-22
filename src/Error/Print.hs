module Error.Print where

import qualified Text.Parsec.Pos as P
import System.IO

type SourcePos = (Int, Int)

data ErrorType 
  = ParserError

pen = 10
aen = 20

printError :: String -> SourcePos -> ErrorType -> String -> IO ()
printError src (l, c) t msg = do
  h <- openFile src ReadMode
  line <- seekLine (l - 1) h
  if c >= length line then compilerError "invalid column in file" else return ()

  -- let chunk = take aen $ drop (max (c - pen) 0) line

  putStrLn $ "_______________________________________"
  case t of
    ParserError -> putStr "ParserError"

  putStrLn $ " at (line " ++ (show l) ++ ", column " ++ (show c) ++ ")"
  putStrLn $ "---------------------------------------"
  putStrLn $ "..." ++ line ++ "..."
  putStrLn $ (replicate (c + 2) ' ') ++ "^"
  putStrLn $ msg
  putStrLn $ "---------------------------------------"
   
  hClose h


compilerError :: String -> IO ()
compilerError s =
  error $ "COMPILER ERROR| " ++ s

seekLine :: Int -> Handle -> IO String
seekLine 0 h = hGetLine h
seekLine n h = hGetLine h >> seekLine (n - 1) h -- handle IO Error here
  
