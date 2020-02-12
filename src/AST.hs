module AST where

type Name 
  = String

type Toplevel 
  = [Either Comb Data]

data Data
  = Data Name [Name] [Const]

data Const 
  = Const Name [Type]

data Comb 
  = Comb Name [Name] Type [Name] Exp

type Alter 
  = (Name, [Name], Exp)

data Exp 
  = EVar Name
  | EPrim Prim 
  | EConst Name 
  | EAp Exp Exp 
  | ELam [Name] Exp 
  | ELet [(Name, Exp)] Exp 
  | ECase Exp [Alter]

data Prim 
  = PInt Int

data Type 
  = TFn Type Type
  | TPrim Name
  | TGen Name
  | TUndef

