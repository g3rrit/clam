module Backend.Cpp.Prelude where

import Data.List
import qualified IR.Builtin as BI

prelude :: String
prelude = intercalate nl $
  [ "// prelude"
  , "typedef int " ++ (show BI.int)
  , "template <class T> T bottom() { exit(-1); }"
  , "// prelude" ++ nl
  ]

nl = "\n"