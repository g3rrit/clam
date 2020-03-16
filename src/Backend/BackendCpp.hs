{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.BackendCpp where

import qualified Data.Map.Lazy as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import System.IO

import IR.IR

backendCpp :: Unit -> IO Bool
backendCpp unit = do
  putStrLn "Backend Cpp"

  forM (M.toList $ umods unit) $ \(i, m) -> do
    (tp, h) <- openTempFile "." $ show i

    e <- runExceptT (runReaderT (codegen m) $ EnvState unit i h)
    case e of
      Left err -> return $ Left err
      Right _  -> return $ Right tp

  return True


class Codegen a where
  cgen :: a -> Env ()

instance Codegen Name where
  cgen (m, d, i) = write $ "__" ++ (show m) ++ "_" ++ (show d) ++ "_" ++ (show i)

instance Codegen Module where
  cgen (Module _ _ ds cs) = do
    -- gen type forward decl
    forM ds $ \(Data n@(m, d, i) t _) -> do
      genTemp m d t
      write "struct "
      cgen n
      write ";"

    -- gen fn forward decl
    forM cs $ \(Comb n@(m, d, i) t as ty _) -> do
      genTemp m d t



    return ()
    where
      genTemp m d (Template t) = do
        write "template <"
        forM [1..t] $ \t' ->
          write "class " >> cgen ((m, d, t') :: Name) >> write ","
        write ">\n"



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
  = ReaderT EnvState (ExceptT CgenErr IO)

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
