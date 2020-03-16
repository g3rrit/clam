module Backend.BackendEval where

import IR.IR

backendEval :: Unit -> IO Bool
backendEval unit = putStrLn "Backend Eval" >> return True
