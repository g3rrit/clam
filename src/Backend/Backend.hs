{-# LANGUAGE LambdaCase #-}

module Backend.Backend where

import Util
import qualified Backend.Cpp.Codegen as Cpp
import qualified Backend.Eval.Eval as Eval
import IR.IR
import Error.Error

backend :: Unit -> RIO BoolError
backend u = do
  c <- config cbackend
  case c of
    BackendCpp -> Cpp.codegen u
    BackendEval -> Eval.eval  u