{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Error.Print where

import qualified Text.Parsec.Pos as P
import System.IO
import Control.Monad

type Pos = (Int, Int)

data Loc = Loc Pos Pos
  deriving (Show)

class Locate a where
  loc :: a -> Loc

instance Locate Loc where
  loc = id

instance Semigroup Loc where
  (Loc s0 e0) <> (Loc s1 e1) = Loc s0 e1

data ErrorType 
  = ParserError

printError :: Locate l => String -> l -> ErrorType -> String -> IO ()
printError src l t msg = do
  let (Loc (l0, c0) (l1, c1)) = loc l
  h <- openFile src ReadMode
  lines <- seekLines (l0 - 1) (l1 - 1) h

  putStrLn $ "_______________________________________"
  case t of
    ParserError -> putStr "ParserError"

  putStrLn $ " at (line " ++ (show l0) ++ ", column " ++ (show c0) ++ ")"
  putStrLn $ "---------------------------------------"
  putStrLn $ (replicate (c0 + (length (show l0)) + 2) ' ') ++ "*"
  forM (zip lines [l0..])
    $ \(line, l) -> putStrLn $ (show l) ++ " | " ++ line
  putStrLn $ (replicate (c1 + (length (show l1)) + 2) ' ') ++ "*"
  putStrLn $ msg
  putStrLn $ "---------------------------------------"
   
  hClose h


compilerError :: String -> IO ()
compilerError s =
  error $ "COMPILER ERROR| " ++ s

seekLines :: Int -> Int -> Handle -> IO [String]
seekLines s e h = seekLines' s e h []
  where seekLines' 0 0 h r = hGetLine h >>= \a -> return $ a : r
        seekLines' 0 e h r = hGetLine h >>= \a -> seekLines' 0 (e - 1) h (a:r)
        seekLines' s e h r = hGetLine h >> seekLines' (s - 1) (e - 1) h r

