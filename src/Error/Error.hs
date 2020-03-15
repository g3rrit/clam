module Error.Error where

import System.Exit

import Error.Print

check :: IO (Maybe a) -> IO a
check a = a >>= (maybe exitFailure return)

panic :: String -> IO a
panic msg = do
  putStrLn $ "Error | " ++ msg
  exitFailure
