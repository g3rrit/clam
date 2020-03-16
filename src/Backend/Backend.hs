{-# LANGUAGE LambdaCase #-}

module Backend.Backend where

import Backend.BackendCpp
import Backend.BackendEval
import IR.IR

backend :: Unit -> String -> IO Bool
backend u = \case
  "cpp" -> backendCpp u
  "eva" -> backendEval u
  a     -> do
    putStrLn $ "ERROR | invalid backend (" ++ a ++ ")"
    return False
