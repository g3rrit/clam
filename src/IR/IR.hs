{-# LANGUAGE LambdaCase #-}

module IR.IR where

import qualified Parser.AST as P
import qualified Data.Map.Strict as M
import IR.Namespace

data Unit
  = Unit
  { uns       :: M.Map Id Integer
  , umods     :: M.Map Integer Module
  } deriving (Show)

data Module
  = Module
  { mid     :: Integer
  , mdatans :: Namespace ()
  , mcombns :: Namespace ()
  , mdata   :: M.Map Name Data
  , mcomb   :: M.Map Name Comb
  } deriving (Show)

data Data
  = SData SumData
  | PData ProData
  deriving (Show)

dname :: Data -> Name
dname = \case
  SData s -> sname s
  PData p -> pname p

data SumData
  = SumData
  { sname :: Name
  , svars :: [ProData]
  } deriving (Show)

data ProData
  = ProData 
  { pname :: Name
  , pmem  :: [Member]
  } deriving (Show)


data Member
  = Member
  { mindex :: Integer
  , mtype  :: Type
  } deriving (Show)

data Comb
  = Comb 
  { cname :: Name 
  , cexp  :: Exp
  } deriving (Show)

data Exp
  = EVar Name Type            -- x
  | ECon Name Type
  | EPrim Prim                 -- 10
  | ESeq Exp Exp               -- exp ; exp
  | EApp Exp Exp               -- fun <int, int>
  | ELet Name Exp              -- Name : Type = Exp doesnt have type any as its just an assignment
  | ELam Lambda                -- \ a -> exp
  | EIf Exp Exp Exp            -- if a then b else if a then b else >> ;
  | ECase Name Exp [Alter]          -- match a | alter | alter end -- needs to have name as assignment
  deriving (Show)

-- allocation
data Field  
  = Field
  { fname :: Name
  , ftype :: Type
  } deriving (Show)

data Lambda
  = Lambda 
  { larg    :: Name 
  , laty    :: Type
  , lfields :: [Field]
  , lexp    :: Exp
  } deriving (Show)

data Alter
  = Alter
  { acons :: Name 
  , alam  :: Lambda
  } deriving (Show)

data Prim
  = PInt Int
  deriving (Show)

data Type
  = TFn Type Type              -- Type -> Type
  | TPrim Name                 -- Bool
  deriving (Show, Eq)

-- expression helper functions

etype :: Exp -> Type
etype = \case 
  EVar _ t -> t
  EPrim p -> eptype p
  ESeq _ e -> etype e  
  EApp f e -> case etype f of 
    TFn te t -> if te /= (etype e) then undefined else t
    _ -> undefined
  ELet _ e -> etype e
  ELam l -> ltype l
  EIf _ t f -> let tt = etype t in if tt /= etype f then undefined else tt
  ECase _ _ (a:_) -> atype a -- rework this
 
eptype :: Prim -> Type
eptype = \case
  PInt _ -> TPrim (Name 0 0 0)

ltype :: Lambda -> Type
ltype l = TFn (laty l) $ etype $ lexp l

ctype :: Comb -> Type
ctype c = etype $ cexp c

atype :: Alter -> Type
atype a = etype $ lexp $ alam a