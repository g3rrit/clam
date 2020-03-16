{-# LANGUAGE TypeApplications #-}

module Pipe where

import Prelude hiding (sequence)
import Control.Monad.Parallel

import Util
import Error.Print
import Error.Error
import Text.PrettyPrint
import Backend.Backend
import Backend.BackendCpp
import Backend.BackendEval
import qualified Parser.Pretty as PP
import qualified Parser.AST as PA
import qualified Parser.Parser as PP
import qualified IR.IR as IR

run :: [File] -> String -> IO ()
run fs args = do
  ms <- check $ parse fs
  putStrLn $ "------ AST -----"
  putStrLn $ concat $ map (render . PP.pretty) ms
  putStrLn $ "------ --- -----"
  u  <- check $ genUnit ms
  checkb $ backend u args

parse :: [File] -> IO (Maybe [PA.Module])
parse fs = sequence <$> (forM fs $ \f -> do
  c <- readFile f
  let r = PP.parse (map (\sl -> if sl == '/' then '.' else sl) f) c
  case r of
    Left (PP.PError p e) -> printError f p ParserError e >> return Nothing
    Right ast -> return $ Just ast)


genUnit :: [PA.Module] -> IO (Maybe IR.Unit)
genUnit ms = return $ Just undefined
