{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Backend.Cpp.Codegen where

import Util
import qualified Data.Map.Lazy as M
import Data.List
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
import IR.Namespace

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
  cgen n = write $ show n

instance Codegen Module where
  cgen m = do
    let ds = mdata m
    let cs = mcomb m

    -- prepend prelude
    write P.prelude

    -- gen type forward decl
    forM_ ds $ \ d -> do
      write "struct "
      cgen $ dname d
      seminl

    -- gen fn forward decl
    forM_ cs $ \ c -> do
      write "extern "
      cgen $ etype $ cexp c
      space
      cgen $ cname c
      seminl

    -- gen type def
    forM_ ds cgen

    -- gen function def
    forM_ cs cgen

    newline
  

instance Codegen Data where 
  cgen = \case 
    SData s -> cgen s
    PData p -> cgen p

instance Codegen ProData where
  cgen p = do
    let n  = pname p
    let ms = pmem p
    write "struct"
    bracket "{" (newline >> forIn seminl ms cgen) "}"
    seminl
    cgenDataCons n ms Nothing

instance Codegen SumData where
  cgen s = do
    let n = sname s
    let ps = svars s
    write "struct"
    cgen n
    space
    bracket "{" 
      ( do
          newline
          write "enum "
          bracket "{" 
            ( do
                newline
                forIn (comma >> newline) ps $ \p -> cgen $ pname p
            ) "}"
          write " _type"
          seminl
          write "union "
          bracket "{"
            ( do 
                newline
                forM_ ps $ \ p -> do
                  write "struct "
                  bracket "{" (newline >> forIn seminl (pmem p) cgen) "}"
                  seminl
            ) "}"
          seminl
      ) "}"
    seminl
    forM_ ps $ \ p -> cgenDataCons (pname p) (pmem p) $ Just n
 
cgenDataCons :: Name -> [Member] -> (Maybe Name) -> Env ()
cgenDataCons n ms mt = do
  let ty = (case mt of
              Just t -> t
              Nothing -> n)
  cgen ty
  space
  write "C_"
  cgen n
  eq
  forM_ ms $ \ m -> do
    write "[]"
    space 
    bracket "(" (cgen m) ")"
    space
    write "{"
    newline
    write "return"
    space

  cgen ty 
  bracket "{" 
    ( do 
        forIn comma ms $ \ m -> 
          let mi = mindex m in write $ "." ++ (show mi) ++ " = " ++ (show mi)
    ) "}"
  seminl

  forM_ ms $ \ _ -> write "}" >> seminl

instance Codegen Member where
  cgen m = do
    let ty = mtype m
    let i  = mindex m
    cgen ty
    space
    write $ show i

instance Codegen Comb where 
  cgen c = do
    let n = cname c
    let t = ctype c
    let e = cexp c
    cgen t
    space
    cgen n
    eq
    cgen e
    seminl

instance Codegen Exp where 
  cgen (EVar n _) = cgen n
  cgen (ECon n _) = write "C_" >> cgen n
  cgen (EPrim p) = cgen p
  cgen (ESeq e0 e1) = bracket "(" (cgen e0 >> comma >> cgen e1) ")"
  cgen (EApp e0 e1) = bracket "(" (cgen e0 >> bracket "(" (cgen e1) ")") ")"
  cgen (ELet n e) = bracket "(" (cgen n >> eq >> cgen e) ")"
  cgen (ELam l) = cgen l
  cgen (EIf c t f) = bracket "(" (cgen c >> colon >> cgen t >> qmark >> cgen f) ")"
  cgen (ECase n e as) = do
    bracket "(" 
      ( do
          bracket "(" (cgen n >> eq >> cgen e) ")"
          comma
          cgenAlter n (etype e) as
      ) ")"

cgenAlter :: Name -> Type -> [Alter] -> Env ()
cgenAlter n t [] = bottom t
cgenAlter n t (a:as) = do
  let an = acons a
  bracket "(" 
    ( do 
        cgen n
        write "._type == "
        case t of
          TFn _ _ -> error "unable to pattern match function"
          TPrim tn -> cgen tn
        write "::"
        cgen an
        write " ? "
        bracket "(" 
          ( do
              cgen $ alam a
              bracket "(" (cgen n >> write ".V_" >> cgen an) ")"
          ) ")"
        write " : "
        cgenAlter n t as
    ) ")"

bottom :: Type -> Env ()
bottom t = do 
  write "undefined"
  bracket "<" (cgen t) ">"
  write "()"

instance Codegen Lambda where
  cgen l = do 
    let arg = larg l
    let aty = laty l
    let fields = lfields l
    let exp = lexp l
    write "[]"
    space
    bracket "(" 
      ( do
          cgen aty
          space
          cgen arg
      ) ")"
    space 
    bracket "{"
      ( do 
          forM_ fields cgen
          write "return "
          cgen exp
          seminl
      ) "}"

instance Codegen Field where
  cgen f = do
    cgen $ ftype f
    space
    cgen $ fname f
    seminl

instance Codegen Prim where 
  cgen (PInt i) = write $ show i

instance Codegen Type where 
  cgen (TFn t0 t1) = do
    write $ "std::function"
    bracket "<" (cgen t1 >> bracket "(" (cgen t0) ")") ">"
  cgen (TPrim n) = cgen n


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
semicolon = write "; "

comma :: Env ()
comma = write ", "

eq :: Env ()
eq = write " = "

colon :: Env ()
colon = write " : "

qmark :: Env ()
qmark = write " ? "

seminl = semicolon >> newline

forIn :: Env t -> [a] -> (a -> Env t) -> Env ()
forIn s t f = (sequence $ intersperse s $ map f t) >> return ()

getMod :: Integer -> Env (Maybe Module)
getMod i = do
  unit <- asks eunit
  return $ M.lookup i $ umods unit

throw :: CgenErr -> Env ()
throw e = lift $ throwE e
