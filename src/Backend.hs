{-# LANGUAGE LambdaCase #-}

module Backend where

import Util
import qualified Backend.Cpp.Codegen as Cpp
import qualified Backend.Eval.Eval as Eval
import Error
import IR.Types

backend :: Unit -> RIO BoolError
backend u = do
  c <- config cbackend
  case c of
    BackendCpp -> Cpp.codegen u
    BackendEval -> Eval.eval  u