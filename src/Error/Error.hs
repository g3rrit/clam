module Error.Error where

import System.Exit

import Error.Print

check :: IO (Maybe a) -> IO a
check a = a >>= (maybe exitFailure return)

checkb :: IO Bool -> IO ()
checkb a = a >>= \a' ->
  if a' then return ()
  else exitFailure

panic :: String -> IO a
panic msg = do
  putStrLn $ "Error | " ++ msg
  exitFailure
