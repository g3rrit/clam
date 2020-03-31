module IR.IR where

import qualified Parser.AST as P
import qualified Data.Map.Lazy as M

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
  = Data Name Variant
  deriving (Show)

data Variant
  = Variant Name [Type]
  deriving (Show)

data Comb
  = Comb Name [Name] Type Exp
  deriving (Show)

type Alter
  = (Name, [Name], Exp)        -- List x xs -> exp

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
  | TKind Type Type            -- Either Type Type
  | TGen Name                  -- a
  deriving (Show)
