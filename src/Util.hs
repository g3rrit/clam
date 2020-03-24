module Util where

import Control.Monad.Trans.Reader
import Control.Monad.Trans

type File
  = String

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
