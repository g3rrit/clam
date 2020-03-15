module Backend.BackendCpp where

import Backend.Backend
import IR.IR

data BackendCpp

instance Backend BackendCpp where
  consume unit = putStrLn "Backend Cpp"
