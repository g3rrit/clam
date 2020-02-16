module AST where

type Name 
  = String

type Toplevel 
  = [Either Comb Data]

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
  | EPar Exp Exp               -- exp ; exp
  | ESeq Exp Exp               -- exp | exp
  | ELet Name (Maybe Type) Exp -- Name : Type = Exp
  | EConst Name                -- True
  | EAp Exp Exp                -- exp exp
  | ELam [Name] Exp            -- [ a b -> exp ]
  | ECase Exp [Alter]          -- match a { > alter > alter }
  deriving (Show)

data Prim 
  = PInt Int
  deriving (Show)

data Type 
  = TFn Type Type              -- Type -> Type
  | TKind Name [Type]          -- Either Type Type
  | TGen Name                  -- a
  deriving (Show)

