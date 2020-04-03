{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.Cpp.Codegen where

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
import Backend.Cpp.Prelude as P

import IR.IR

codegen :: Unit -> RIO Bool
codegen unit = do
  liftIO $ putStrLn "Backend Cpp"

  r <- forM (M.toList $ umods unit) $ \(i, m) -> do
    (tp, h) <- liftIO $ openTempFile "." $ show i ++ ".cpp"

    liftIO $ putStrLn $ "writing to: " ++ tp
    liftIO $ putStrLn "------------------------"

    e <- runExceptT (runReaderT 
          (cgen m 
            >> when (i == i) cgenMain) $ EnvState unit i h) -- TODO: chance to i == 0

    liftIO $ putStrLn "________________________"

    liftIO $ hClose h
    case e of
      Left err -> return $ Left (tp, err)
      Right _  -> return $ Right tp

  let es = concatMap (\c -> case c of Left (_, err) -> [err] ;Right _ -> []) r
  let ts = concatMap (\c -> case c of Left (tp, _) -> [tp] ;Right tp -> [tp]) r

  r <- (if length es >= 1 
        then (liftIO $ putStrLn "Error") >> return False
        else execCC ts) 
       
  cleanup ts
  return r

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

    -- prepend prelude
    write P.prelude

    -- gen type forward decl
    forM_ ds $ \(Data n _) -> do
      write "struct "
      cgen n
      seminl

    -- gen fn forward decl
    forM_ cs $ \(Comb n ty _) -> do
      write "extern "
      cgen ty
      cgen n
      seminl

    -- gen type def
    forM_ ds $ \(Data n vs) -> do
      write "struct "
      cgen n
      bracket "{" (cgen vs) "}"
      seminl

    -- gen function def
    forM_ cs $ \(Comb n ty exp) -> do
      cgen ty
      cgen n
      eq
      cgen exp
      seminl
  

instance Codegen Data where 
  cgen d = undefined

instance Codegen [Variant] where 
  cgen vs = undefined

instance Codegen Comb where 
  cgen c = undefined

instance Codegen Exp where 
  cgen (EVar n) = cgen n >> space
  cgen (EPrim p) = cgen p >> space
  cgen (ESeq e0 e1) = bracket "(" (cgen e0 >> comma >> cgen e1) ")"
  cgen (EApp e0 e1) = bracket "(" (cgen e0 >> bracket "(" (cgen e1) ")") ")"
  cgen (ELet n e) = bracket "(" (cgen n >> eq >> cgen e) ")"
  cgen (ELam l) = cgen l
  cgen (EIf c t f) = bracket "(" (cgen c >> colon >> cgen t >> qmark >> cgen f) ")"
  cgen (ECase e as) = undefined

instance Codegen Lambda where
  cgen l = undefined

instance Codegen Alter where 
  cgen a = undefined

instance Codegen Prim where 
  cgen (PInt i) = write $ show i

instance Codegen Type where 
  cgen (TFn t0 t1) = do
    write $ "std::function"
    bracket "<" (cgen t1 >> bracket "(" (cgen t0) ")") ">"
    space
  cgen (TPrim n) = cgen n >> space



cgenMain :: Env ()
cgenMain = write "int main(int argc, char **argv) { return 0; }\n"

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
  liftIO $ putStr t -- testing

space :: Env ()
space = write " "

newline :: Env ()
newline = write "\n"

bracket :: String -> Env t -> String -> Env t
bracket l e r = write l *> e <* write r

semicolon :: Env ()
semicolon = write ";"

comma :: Env ()
comma = write ","

eq :: Env ()
eq = write " = "

colon :: Env ()
colon = write " : "

qmark :: Env ()
qmark = write " ? "

seminl = semicolon >> newline

getMod :: Integer -> Env (Maybe Module)
getMod i = do
  unit <- asks eunit
  return $ M.lookup i $ umods unit

throw :: CgenErr -> Env ()
throw e = lift $ throwE e
