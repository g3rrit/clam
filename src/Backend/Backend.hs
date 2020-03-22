{-# LANGUAGE LambdaCase #-}

module Backend.Backend where

import Util
import Backend.BackendCpp
import Backend.BackendEval
import IR.IR

backend :: Unit -> RIO Bool
backend u = do
  c <- config cbackend
  case c of
    BackendCpp -> backendCpp u
    BackendEval -> backendEval u