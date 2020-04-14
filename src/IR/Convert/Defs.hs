{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.Convert.Defs where

import Error

import qualified IR.Types as IR
import qualified IR.Namespace as IR
import qualified AST.Types as AST

import qualified Data.Map.Strict as M
import Data.Either


import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import qualified Control.Monad.State.Class as SC
import qualified Control.Monad.Error.Class as EC


convertModule :: Integer -> IR.Unit -> AST.Module -> IR.Module -> EitherError IR.Module
convertModule id u ma mi = 
  Right $ IR.Module 
  { IR.mid   = id
  , IR.mdatans = undefined
  , IR.mcombns = undefined
  , IR.mconsns = undefined
  , IR.mdata = M.fromList $ map (\d -> (IR.dname d, d)) ds
  , IR.mcomb = M.fromList $ map (\c -> (IR.cname c, c)) cs
  }
  where 
    (cs, ds) = let (cs', ds') = partitionEithers $ AST.ms ma in (map convertComb cs', map convertData ds')

-- TODO

convertData :: AST.Data -> IR.Data
convertData _ = undefined

convertComb :: AST.Comb -> IR.Comb
convertComb _ = undefined

-- MONAD STACK

data EnvState
  = EnvState
  { envUnit  :: IR.Unit
  , envMod   :: IR.Module
  }

newtype Env a
  = Env (StateT EnvState (Except Error) a)
  deriving (Functor, Applicative, Monad, SC.MonadState EnvState, EC.MonadError Error)

runEnv :: EnvState -> Env a -> Either Error a
runEnv is (Env s) = runExcept $ evalStateT s is

