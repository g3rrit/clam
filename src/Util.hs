{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util 
  ( module Util
  , module Control.Monad.IO.Class
  ) where

import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans
import Data.List

import Control.Monad.IO.Class
import qualified Control.Monad.Reader.Class as RC

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

newtype RIO a
  = RIO 
  { runReader :: R.ReaderT Config IO a
  } deriving (Functor, Applicative, Monad, RC.MonadReader Config, MonadIO)

run :: Config -> RIO a -> IO a 
run c r = R.runReaderT (runReader r) c

config :: (Config -> b) -> RIO b
config = RC.reader
