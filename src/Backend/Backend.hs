{-# LANGUAGE AllowAmbiguousTypes #-}

module Backend.Backend where

import IR.IR

class Backend a where
  consume :: Unit -> IO ()
