module IR.IR where

import qualified Parser.AST as P
import qualified Data.Map.Strict as M

type Name
  = (Integer, Integer, Integer) -- (module, comb/data, id)

type Id
  = String

data Unit
  = Unit
  { uns   :: M.Map Id Integer
  , umods :: M.Map Integer Module
  } deriving (Show)

data Module
  = Module
  { mid    :: Integer
  , mdata  :: M.Map Name Data
  , mcomb  :: M.Map Name Comb
  } deriving (Show)

data Data
  = Data 
  { dname :: Name 
  , dvars :: [Variant]
  } deriving (Show)

data Variant
  = Variant 
  { vname  :: Name 
  , vtypes :: [Type]
  } deriving (Show)

data Comb
  = Comb 
  { cname :: Name 
  , ctype :: Type  -- combinator doesn't have args as xexp is made up of lambdas
  , cexp  :: Exp
  } deriving (Show)

data Alter
  = Alter
  { acons :: Name 
  , aargs :: [Name] 
  , aexp  :: Exp
  } deriving (Show)

data Exp
  = EVar Name                  -- x
  | EPrim Prim                 -- 10
  | ESeq Exp Exp               -- exp ; exp
  | EApp Exp Exp               -- fun <int, int>
  | ELet Name Type Exp         -- Name : Type = Exp
  | ELam [Name] Exp            -- \ a b -> exp
  | EIf Exp Exp Exp            -- if a then b else if a then b else >> ;
  | ECase Exp [Alter]          -- match a | alter | alter end
  deriving (Show)

data Prim
  = PInt Int
  deriving (Show)

data Type
  = TFn Type Type              -- Type -> Type
  | TPrim Name                 -- Bool
  deriving (Show)
