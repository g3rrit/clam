module IR.AST where

import qualified Parser.AST as P
import qualified Data.Map.Lazy as M

type Name
  = (Integer, Integer, Integer) -- (module, comb/data, id)

type Id
  = String

type Tag
  = Tag Name Template Type

type Namespace
  = M.Map Id Tag

data Template
  = Template Int
   deriving (Show)

data Unit
  = Unit
  { uns   :: M.Map Id Integer
  , umods :: M.Map Integer Module
  } deriving (Show)

data Module
  = Module
  { mid   :: Integer
  , mns   :: Namespace
  , mdata :: M.Map Name Data
  , mcomb :: M.Map Name Comb
  } deriving (Show)

data Data
  = Data Name Template [Name] Variant
  deriving (Show)

data Variant
  = Variant Name [Type]
  deriving (Show)

data Comb
  = Comb Name Template [Name] Type Exp
  deriving (Show)

data Env
  = Env (Name ->

type Alter
  = (Name, [Name], Exp)        -- List x xs -> exp

data Exp
  = ECall Name [Type] [Exp]    -- fun <int, int> 10 10
  | EVar Name                  -- x
  | EPrim Prim                 -- 10
  | ESeq Exp Exp               -- exp ; exp
  | ELet Name Type Exp         -- Name : Type = Exp
  | EConst Name                -- True
  | EAp Exp Exp                -- exp exp
  | ELam [Name] Exp            -- [ a b -> exp ]
  | EIf Exp Exp Exp            -- if a then b else c | if a then b con
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
