module IR.AST where

import qualified Parser.AST as P
import qualified Data.Map.Lazy as M

type Name 
  = (Integer, Integer, Integer) -- (module, comb/data, id)

type Id 
  = String

type Namespace 
  = M.Map Id Name

data Module 
  = Module 
  { mns   :: Namespace 
  , mdata :: M.Map Id Data
  , mcomb :: M.Map Id Comb
  } 

data Data
  = Data Name [Name] [Variant] -- data List a = Var | Var
  deriving (Show)

data Variant 
  = Variant Name [Type]        -- List a (List a)
  deriving (Show)

data Comb 
  = Comb Name [Name] Type Exp  -- let foo a b : Type = exp
  deriving (Show)

type Alter 
  = (Name, [Name], Exp)        -- List x xs -> exp

data Exp 
  = EVar Name                  -- x
  | EPrim Prim                 -- 10
  | ESeq Exp Exp               -- exp ; exp
  | ELet Name (Maybe Type) Exp -- Name : Type = Exp
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


