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

data Namespace
  = Namespace (M.Map Id Name)
  deriving (Show)

class Searchable a i where
  search :: i -> a -> Maybe Name

instance Searchable Namespace Id where
  search i (Namespace m) = M.lookup i m

instance (Foldable t, Searchable s i) => Searchable (t s) i where
  search i ts = asum (map (search i) $ toList ts)