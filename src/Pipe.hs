{-# LANGUAGE TypeApplications #-}

module Pipe where

import Util
import Error.Error
import Text.PrettyPrint
import Backend.Backend
import qualified Parser.Pretty as PP
import qualified Parser.AST as AST
import qualified Parser.Parser as PP
import qualified IR.IR as IR
import qualified IR.Convert as C
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans

pipe :: [File] -> RIO ()
pipe fs = do
  ms <- (check $ parse fs) 
  liftIO $ putStrLn $ "------ AST -----"
  liftIO $ putStrLn $ concatMap (render . PP.pretty) ms
  liftIO $ putStrLn $ "------ --- -----"
  u  <- check $ genUnit ms
  check $ backend u

parse :: [File] -> RIO [EitherError AST.Module]
parse fs = sequence <$> (forM fs $ \f -> do
  c <- liftIO $ readFile f
  return $ PP.parse (map (\sl -> if sl == '/' then '.' else sl) f) c

-- todo fix
genUnit :: [AST.Module] -> RIO (EitherError IR.Unit)
genUnit ms = return $ Just $ IR.Unit 
  { IR.uns   = M.fromList [("main", 1)]
  , IR.umods = M.fromList $ map (\m' -> (2, m')) ims
  }
  where ims = map (\m -> case C.convert 0 m of 
                          Left m' -> m') ms
