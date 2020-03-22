module Error.Error where

import System.Exit

import Util
import Control.Monad.Trans
import Error.Print

check :: RIO (Maybe a) -> RIO a
check a = a >>= (maybe (liftIO exitFailure) return)

checkb :: RIO Bool -> RIO ()
checkb a = a >>= \a' ->
  if a' then return ()
  else liftIO exitFailure

panic :: String -> RIO a
panic msg = do
  liftIO $ putStrLn $ "Error | " ++ msg
  liftIO exitFailure
