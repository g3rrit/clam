{-# LANGUAGE QuasiQuotes #-}

module Backend.Cpp.Prelude where

import Text.RawString.QQ

prelude :: String
prelude = [r|// prelude
typedef int __0_0_0;
// prelude
|]