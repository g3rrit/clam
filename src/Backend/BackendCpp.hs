{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.BackendCpp where

import Util
import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.IO

import IR.IR

backendCpp :: Unit -> RIO Bool
backendCpp unit = do
  liftIO $ putStrLn "Backend Cpp"

  r <- forM (M.toList $ umods unit) $ \(i, m) -> do
    (tp, h) <- liftIO $ openTempFile "." $ show i

    e <- runExceptT (runReaderT (codegen m) $ EnvState unit i h)
    hClose h
    case e of
      Left err -> return $ Left (tp, err)
      Right _  -> return $ Right tp

  let es = concat $ map (\c -> case c of Left (_, err) -> [err] ;Right _ -> []) r
  let ts = concat $ map (\c -> case c of Left (tp, _) -> [tp] ;Right tp -> [tp]) r

  -- compile files here
  return True
  where 
    cleanup ts = undefined

class Codegen a where
  cgen :: a -> Env ()

instance Codegen Name where
  cgen (m, d, i) = write $ "__" ++ (show m) ++ "_" ++ (show d) ++ "_" ++ (show i)

instance Codegen Module where
  cgen m = do
    let ds = mdata m
    let cs = mcomb m
    -- gen type forward decl
    forM ds $ \(Data n _) -> do
      write "struct "
      cgen n
      write ";"

    -- gen fn forward decl
    forM cs $ \(Comb n as ty _) -> do
      return ()



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
