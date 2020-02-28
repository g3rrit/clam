{-# LANGUAGE LambdaCase #-}

module Parser.AST where

import Error.Print

type Name 
  = String

data Module 
  = Module String [Either Comb Data]

data Data
  = Data Name [Name] [Variant] Loc -- data List a = Var | Var
  deriving (Show)

instance Locate Data where
  loc (Data _ _ _ l) = l

data Variant 
  = Variant Name [Type] Loc       -- List a (List a)
  deriving (Show)

instance Locate Variant where
  loc (Variant _ _ l) = l

data Comb 
  = Comb Name [Name] Type Exp Loc  -- let foo a b : Type = exp
  deriving (Show)

instance Locate Comb where
  loc (Comb _ _ _ _ l) = l

data Alter 
  = Alter Name [Name] Exp Loc        -- List x xs -> exp
  deriving (Show)

instance Locate Alter where
  loc (Alter _ _ _ l) = l

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

instance Locate Exp where
  loc = \case 
    EVar _ l     -> l
    EPrim _ l    -> l
    ELam _ _ l   -> l
    EIf _ _ _ l  -> l
    ECase _ _ l  -> l
    ELet _ _ _ l -> l
    ESeq l r     -> (loc l) <> (loc r)
    EAp l r      -> (loc l) <> (loc r)

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

instance Locate Type where
  loc = \case 
    TFn l r   -> (loc l) <> (loc r)
    TPrim _ l -> l
    TKind l r -> (loc l) <> (loc r)
    TGen _ l  -> l
    TRef t    -> loc t
    TUptr t   -> loc t
    TSptr t   -> loc t

