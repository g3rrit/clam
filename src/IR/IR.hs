{-# LANGUAGE LambdaCase #-}

module IR.IR where

import qualified Parser.AST as P
import qualified Data.Map.Strict as M
import IR.Namespace

type Uid 
  = Integer

data Unit
  = Unit
  { uns       :: M.Map Id Uid
  , umods     :: M.Map Uid Module
  } deriving (Show)

data Module
  = Module
  { mid     :: Integer
  , mdatans :: Namespace
  , mcombns :: Namespace
  , mconsns :: Namespace
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

-- this nodes does only appear when 
-- searching for constructors with searchCons
-- and is constructed from prodata
data Cons
  = Cons
  { coname :: Name
  , cotype :: Type
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

-- UTIL

mlookup :: Name -> Module -> (Module -> M.Map Name a) -> Maybe a
mlookup n m f = M.lookup n (f m)

mlookupData :: Name -> Module -> Maybe Data
mlookupData n m = mlookup n m mdata

mlookupComb :: Name -> Module -> Maybe Comb
mlookupComb n m = mlookup n m mcomb

mlookupCons :: Name -> Module -> Maybe Cons
mlookupCons n m = mlookup n m mdata >>= \d ->
  case d of 
    SData _ -> Nothing
    PData p -> Just $ Cons (pname p) $ foldr (\m b -> TFn (mtype m) b) (TPrim $ dname d) $ pmem p


ulookup :: Uid -> Name -> Unit -> (Name -> Module -> Maybe a) -> Maybe a
ulookup i n u f = M.lookup i (umods u) >>= \m -> f n m

ulookupData :: Uid -> Name -> Unit -> Maybe Data
ulookupData i n u = ulookup i n u mlookupData

ulookupComb :: Uid -> Name -> Unit -> Maybe Comb
ulookupComb i n u = ulookup i n u mlookupComb

ulookupCons :: Uid -> Name -> Unit -> Maybe Cons
ulookupCons i n u = ulookup i n u mlookupCons


msearch :: Id -> Module -> (Module -> Namespace) -> (Name -> Module -> Maybe a) -> Maybe a
msearch i m ns f = search i (ns m) >>= \n -> f n m 

msearchData :: Id -> Module -> Maybe Data
msearchData i m = msearch i m mdatans mlookupData

msearchComb :: Id -> Module -> Maybe Comb
msearchComb i m = msearch i m mcombns mlookupComb

msearchCons :: Id -> Module -> Maybe Cons
msearchCons i m = msearch i m mconsns mlookupCons


usearch :: Id -> Id -> Unit -> (Module -> Namespace) -> (Name -> Module -> Maybe a) -> Maybe a
usearch ui i u ns f = M.lookup ui (uns u) >>= \mi -> M.lookup mi (umods u) >>= \m -> msearch i m ns f

usearchData :: Id -> Id -> Unit -> Maybe Data
usearchData ui i u = usearch ui i u mdatans mlookupData

usearchComb :: Id -> Id -> Unit -> Maybe Comb
usearchComb ui i u = usearch ui i u mcombns mlookupComb

usearchCons :: Id -> Id -> Unit -> Maybe Cons
usearchCons ui i u = usearch ui i u mconsns mlookupCons

