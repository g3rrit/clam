{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.Convert.Names where

import qualified Parser.AST as AST
import qualified IR.IR as IR
import qualified IR.Namespace as IR
import qualified Data.Map.Strict as M
import Data.Either
import Data.Foldable
import Error

import Control.Monad.Trans.State

data Names
  = Names IR.Id IR.Name IR.Name IR.Name

fillNamespace :: Integer -> AST.Module -> EitherError IR.Module
fillNamespace id m = Right $ IR.Module -- throw error if names appear duplicate maybe
  { IR.mid = id
  , IR.mdatans = nsdata ns
  , IR.mcombns = nscomb ns
  , IR.mconsns = nscons ns
  , IR.mdata = M.empty 
  , IR.mcomb = M.empty
  }
  where 
    ns = execState (getNames id $ AST.ms m) emptyNS

getNames :: Integer -> [Either AST.Comb AST.Data] -> Namestate ()
getNames id = traverse_ $ \ cd -> next >>= \ i -> getName i cd
  where
    getName i (Left c)  = insertNSComb (AST.cname c) $ IR.Name id i 0
    getName i (Right (AST.SData d)) = (insertNSData (AST.sname d) $ IR.Name id i 0) 
                                    >> (traverse_ (\ p -> next >>= \ i -> getNameP i p) $ AST.svars d)
    getName i (Right (AST.PData d)) = (insertNSData (AST.pname d) $ IR.Name id i 0) >> getNameP i d
    getNameP i p = insertNSCons (AST.pname p) $ IR.Name id i 0

data NS
  = NS 
  { nsi :: Integer
  , nsdata :: IR.Namespace
  , nscomb :: IR.Namespace 
  , nscons :: IR.Namespace
  }

emptyNS = NS 1 M.empty M.empty M.empty 

type Namestate = State NS

next = gets nsi >>= \ i -> (modify $ \ (NS _ d c co) -> NS (i + 1) d c co) >> (return $ i + 1)

insertNSData s n = modify $ \ (NS i d c co) -> NS i (M.insert s n d) c co
insertNSComb s n = modify $ \ (NS i d c co) -> NS i d (M.insert s n c) co 
insertNSCons s n = modify $ \ (NS i d c co) -> NS i d c (M.insert s n co)

