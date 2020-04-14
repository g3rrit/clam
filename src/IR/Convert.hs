{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Convert where

import qualified Parser.AST as AST
import qualified IR.IR as IR
import qualified IR.Namespace as IR
import qualified Data.Map.Strict as M
import Data.Either
import Error

convert :: [AST.Module] -> EitherError IR.Unit
convert = undefined