{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Error.Error where

import System.Exit
import System.IO
import qualified Text.Parsec.Pos as P
import Control.Monad
import Util
import Control.Monad.Trans

type Pos = (Int, Int)

data Loc = Loc Pos Pos
  deriving (Show)

class Locate a where
  loc :: a -> Loc

instance Locate Loc where
  loc = id

instance Semigroup Loc where
  (Loc s0 e0) <> (Loc s1 e1) = Loc s0 e1
  
data Error
  = Error 
  { errType :: ErrorType 
  , errFile :: File 
  , errMsg  :: String 
  , errLoc  :: Loc
  }  

data ErrorType 
  = ParserError 
  | ConvertError 
  | BackendError
  deriving (Show)

type MaybeError 
  = Maybe Error

type EitherError a
  = Either Error a

type BoolError
  = Bool

class Checkable a b where
  check :: a -> RIO b

instance Checkable MaybeError () where
  check = \case
    Just e -> exitError e
    Nothing -> return () 

instance Checkable (EitherError a) a where
  check = \case
    Left e -> exitError e
    Right a -> return a

instance Checkable BoolError () where
  check = \case
    True -> return ()
    False -> liftIO $ exitFailure

instance Checkable a b => Checkable (RIO a) b where
  check a = a >>= check

instance (Checkable a b, Traversable t) => Checkable (RIO (t a)) (t b) where
  check mas = mas >>= (traverse check)

exitError :: Error -> RIO a
exitError e = do
  liftIO $ printError e
  liftIO $ exitFailure 

printError :: Error -> IO ()
printError err = do
  let src = toPath $ errFile err
  let l   = errLoc err
  let t   = errType err
  let msg = errMsg err
  let (Loc (l0, c0) (l1, c1)) = loc l
  h <- openFile src ReadMode
  lines <- seekLines (l0 - 1) (l1 - 1) h

  putStrLn $ "_______________________________________"
  putStr $ show t

  putStrLn $ " in " ++ src
  putStrLn $ "at (line " ++ (show l0) ++ ", column " ++ (show c0) ++ ")"
  putStrLn $ "---------------------------------------"
  putStrLn $ (replicate (c0 + (length (show l0)) + 2) ' ') ++ "*"
  forM (zip lines [l0..])
    $ \(line, l) -> putStrLn $ (show l) ++ " | " ++ line
  putStrLn $ (replicate (c1 + (length (show l1)) + 2) ' ') ++ "*"
  putStrLn $ msg
  putStrLn $ "---------------------------------------"
   
  hClose h

seekLines :: Int -> Int -> Handle -> IO [String]
seekLines s e h = seekLines' s e h []
  where seekLines' 0 0 h r = hGetLine h >>= \a -> return $ a : r
        seekLines' 0 e h r = hGetLine h >>= \a -> seekLines' 0 (e - 1) h (a:r)
        seekLines' s e h r = hGetLine h >> seekLines' (s - 1) (e - 1) h r


-- UTIL
panic :: String -> RIO a
panic msg = do
  liftIO $ putStrLn $ "COMPILER ERROR | " ++ msg
  liftIO exitFailure

