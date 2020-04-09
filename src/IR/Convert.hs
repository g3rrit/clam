{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.Convert where

import qualified Parser.AST as AST
import qualified IR.IR as IR
import qualified IR.Namespace as IR
import qualified Data.Map.Strict as M
import Data.Either
import Error.Error

import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import qualified Control.Monad.State.Class as SC
import qualified Control.Monad.Error.Class as EC

convert :: Integer -> AST.Module -> EitherError IR.Module
convert id m = Right $ IR.Module 
  { IR.mid   = id
  , IR.mdata = M.fromList $ map (\d -> (IR.dname d, d)) ds
  , IR.mcomb = M.fromList $ map (\c -> (IR.cname c, c)) cs
  }
  where 
    (cs, ds) = let (cs', ds') = partitionEithers $ AST.ms m in (map convertComb cs', map convertData ds')

-- TODO

convertData :: AST.Data -> IR.Data
convertData _ = IR.SData $ IR.SumData (IR.Name 2 2 2) [IR.ProData (IR.Name 3 3 3) [IR.Member 0 $ IR.TPrim (IR.Name 1 1 1)]]

convertComb :: AST.Comb -> IR.Comb
convertComb _ = IR.Comb (IR.Name 2 2 2) (IR.EVar (IR.Name 4 4 4) (IR.TPrim (IR.Name 3 3 3))) 

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

