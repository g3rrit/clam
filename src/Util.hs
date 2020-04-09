module Util where

import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Data.List

type File
  = [String]

toPath :: [String] -> String
toPath = intercalate "/"

data Config
  = Config 
  { cversion :: String
  , cpath    :: String
  , cbackend :: Backend
  , coutput  :: String
  , ccc      :: String
  }

data Backend 
  = BackendCpp
  | BackendEval

type RIO 
  = ReaderT Config IO

run :: Config -> RIO a -> IO a 
run c r = runReaderT r c

config :: (Config -> b) -> RIO b
config = reader
