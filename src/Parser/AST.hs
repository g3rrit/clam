{-# LANGUAGE LambdaCase #-}

module Parser.AST where

import Error.Print

type Name
  = String

data Module
  = Module 
  { mname :: String 
  , ms    :: [Either Comb Data]
  } deriving (Show)

data Data
  = Data 
  { dname :: Name 
  , dvars :: [Variant]
  , dloc  :: Loc 
  } deriving (Show)

data Variant
  = Variant 
  { vname  :: Name 
  , vtypes :: [Type] 
  , vloc   :: Loc       -- List a (List a)
  } deriving (Show)

data Comb
  = Comb 
  { cname :: Name 
  , cargs :: [Name] 
  , ctype :: Type 
  , cexp  :: Exp 
  , cloc  :: Loc  -- let foo a b : Type = exp
  } deriving (Show)

data Alter
  = Alter 
  { acons :: Name 
  , aargs :: [Name] 
  , aexp  :: Exp 
  , aloc  :: Loc        -- List x xs -> exp
  } deriving (Show)

data Exp
  = EPrim Prim Loc                 -- 10
  | EVar Name Loc                  -- n
  | ELam [Name] Exp Loc            -- \ a b -> exp
  | EIf Exp Exp Exp Loc            -- if a then b else if a then b else >>
  | ECase Exp [Alter] Loc          -- match a | alter | alter end
  | ELet Name (Maybe Type) Exp Loc -- Name : Type = Exp
  | ESeq Exp Exp                   -- exp ; exp
  | EAp Exp Exp                    -- exp exp
  deriving (Show)

data Prim
  = PInt Int
  | PVar Name
  deriving (Show)

data Type
  = TFn Type Type                 -- Type -> Type
  | TPrim Name Loc                -- Bool
  | TRef Type                  -- &Type
  | TUptr Type                 -- ^Type
  | TSptr Type                 -- *Type
  deriving (Show)

instance Locate Data where
  loc (Data _ _ l) = l

instance Locate Variant where
  loc (Variant _ _ l) = l

instance Locate Comb where
  loc (Comb _ _ _ _ l) = l

instance Locate Alter where
  loc (Alter _ _ _ l) = l

instance Locate Exp where
  loc = \case
    EPrim _ l    -> l
    ELam _ _ l   -> l
    EIf _ _ _ l  -> l
    ECase _ _ l  -> l
    ELet _ _ _ l -> l
    ESeq l r     -> (loc l) <> (loc r)
    EAp l r      -> (loc l) <> (loc r)

instance Locate Type where
  loc = \case
    TFn l r   -> (loc l) <> (loc r)
    TPrim _ l -> l
    TRef t    -> loc t
    TUptr t   -> loc t
    TSptr t   -> loc t
