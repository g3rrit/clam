module Backend.Eval.Eval where

import Util
import Control.Monad.Trans
import IR.IR

eval :: Unit -> RIO Bool
eval unit = (liftIO $ putStrLn "Backend Eval") >> return True
