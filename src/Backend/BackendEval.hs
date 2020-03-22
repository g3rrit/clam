module Backend.BackendEval where

import Util
import Control.Monad.Trans
import IR.IR

backendEval :: Unit -> RIO Bool
backendEval unit = (liftIO $ putStrLn "Backend Eval") >> return True
