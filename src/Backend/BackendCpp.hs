{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.BackendCpp where

import Util
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.IO
import System.Directory
import System.Process
import System.Exit

import IR.IR

backendCpp :: Unit -> RIO Bool
backendCpp unit = do
  liftIO $ putStrLn "Backend Cpp"

  r <- forM (M.toList $ umods unit) $ \(i, m) -> do
    (tp, h) <- liftIO $ openTempFile "." $ show i

    e <- runExceptT (runReaderT (codegen m) $ EnvState unit i h)
    liftIO $ hClose h
    case e of
      Left err -> return $ Left (tp, err)
      Right _  -> return $ Right tp

  let es = concatMap (\c -> case c of Left (_, err) -> [err] ;Right _ -> []) r
  let ts = concatMap (\c -> case c of Left (tp, _) -> [tp] ;Right tp -> [tp]) r

  (if length es >= 1 
  then (liftIO $ putStrLn "Error") >> return False
  else execCC ts) <* cleanup ts

  where 
    cleanup ts = forM_ ts (liftIO . removeFile)

execCC :: [FilePath] -> RIO Bool
execCC ts = do
  cc <- config ccc
  o  <- config coutput
  e  <- liftIO $ system $ cc ++ " -o " ++ o ++ (concatMap (" " ++) ts)
  case e of 
    ExitSuccess    -> return True
    ExitFailure e' -> (liftIO $ putStrLn $ "[" ++ cc ++ "] exited with error: " ++ (show e)) >> return False

class Codegen a where
  cgen :: a -> Env ()

instance Codegen Name where
  cgen (m, d, i) = write $ "__" ++ (show m) ++ "_" ++ (show d) ++ "_" ++ (show i)

instance Codegen Module where
  cgen m = do
    let ds = mdata m
    let cs = mcomb m
    -- gen type forward decl
    forM_ ds $ \(Data n _) -> do
      write "struct "
      cgen n
      write ";"

    -- gen fn forward decl
    forM_ cs $ \(Comb n as ty _) -> do
      return ()




codegen :: Module -> Env ()
codegen m = undefined

-- Data Types

data CgenErr
  = CgenErr String

data EnvState
  = EnvState
  { eunit :: Unit
  , eid   :: Integer
  , eh    :: Handle
  }

type Env
  = ReaderT EnvState (ExceptT CgenErr RIO)

write :: String -> Env ()
write t = do
  h <- asks eh
  liftIO $ hPutStr h t

getMod :: Integer -> Env (Maybe Module)
getMod i = do
  unit <- asks eunit
  return $ M.lookup i $ umods unit

throw :: CgenErr -> Env ()
throw e = lift $ throwE e
