module Pipe where

import Prelude hiding (sequence)
import System.Exit
import Control.Monad.Parallel

import Util
import Error.Print
import Text.PrettyPrint
import qualified Parser.Pretty as PP
import qualified Parser.AST as PA
import qualified Parser.Parser as PP
import qualified IR.AST as IA

check :: IO (Maybe a) -> IO a
check a = a >>= (maybe exitFailure return)

compile :: [File] -> IO ()
compile fs = do
  ms <- check $ parse fs
  putStrLn $ "------ AST -----"
  putStrLn $ concat $ map (render . PP.pretty) ms
  putStrLn $ "------ --- -----"
  u  <- check $ genUnit ms
  check $ genCode u

parse :: [File] -> IO (Maybe [PA.Module])
parse fs = sequence <$> (forM fs $ \f -> do
  c <- readFile f
  let r = PP.parse (map (\sl -> if sl == '/' then '.' else sl) f) c
  case r of
    Left (PP.PError p e) -> printError f p ParserError e >> return Nothing
    Right ast -> return $ Just ast)


genUnit :: [PA.Module] -> IO (Maybe IA.Unit)
genUnit ms = return Nothing

genCode :: IA.Unit -> IO (Maybe ())
genCode u = return Nothing
