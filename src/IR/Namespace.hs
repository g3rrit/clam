{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Namespace where

import qualified Data.Map.Strict as M
import Data.Foldable

data Name
   = Name Integer Integer Integer -- (module, comb/data, id)
   deriving (Eq, Ord)

instance Show Name where
  show (Name m c i) = "__" ++ (show m) ++ "_" ++ (show c) ++ "_" ++ (show i)

type Id
  = String

data Namespace a
  = Namespace (M.Map Id (Name, a))
  deriving (Show)

class Searchable a b i where
  search :: i -> a -> Maybe (Name, b)

instance Searchable (Namespace a) a Id where
  search i (Namespace m) = M.lookup i m

instance (Foldable t, Searchable s a i) => Searchable (t s) a i where
  search i ts = asum (map (search i) $ toList ts)