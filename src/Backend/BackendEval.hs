module Backend.BackendEval where

import Backend.Backend

data BackendEval

instance Backend BackendEval where
  consume unit = putStrLn "Backend Eval" >> return True
