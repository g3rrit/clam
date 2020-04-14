{-# LANGUAGE TypeApplications #-}

module Pipe where

import Util
import Error
import Text.PrettyPrint
import Backend
import qualified AST.Pretty as PP
import qualified AST.Types as AST
import qualified AST.Parser as PP
import qualified IR.Types as IR
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
  u  <- check $ C.convert ms
  check $ backend u

parse :: [File] -> RIO [EitherError AST.Module]
parse fs = forM fs $ \f -> do
  c <- liftIO $ readFile $ toPath f
  return $ PP.parse f c