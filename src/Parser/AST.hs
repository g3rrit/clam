module Parser.AST where

import Error.Print

type Name 
  = String

data Module 
  = Module String [Either Comb Data]

data Data
  = Data Name [Name] [Variant] Loc -- data List a = Var | Var
  deriving (Show)

data Variant 
  = Variant Name [Type]        -- List a (List a)
  deriving (Show)

data Comb 
  = Comb Name [Name] Type Exp Loc  -- let foo a b : Type = exp
  deriving (Show)

data Alter 
  = Alter Name [Name] Exp Loc        -- List x xs -> exp
  deriving (Show)

data Exp 
  = EVar Name Loc              -- x
  | EPrim Prim Loc             -- 10
  | EConst Name Loc               -- True
  | ELam [Name] Exp Loc           -- [ a b -> exp ]
  | EIf Exp Exp Exp Loc           -- if a then b else c | if a then b con
  | ECase Exp [Alter] Loc          -- match a | alter | alter end
  | ELet Name (Maybe Type) Exp Loc -- Name : Type = Exp
  | ESeq Exp Exp               -- exp ; exp
  | EAp Exp Exp                -- exp exp
  deriving (Show)

data Prim 
  = PInt Int
  deriving (Show)

data Type 
  = TFn Type Type              -- Type -> Type
  | TPrim Name Loc                -- Bool
  | TKind Type Type            -- Either Type Type
  | TGen Name Loc                 -- a
  | TRef Type                  -- &Type
  | TUptr Type                 -- ^Type
  | TSptr Type                 -- *Type
  deriving (Show)

