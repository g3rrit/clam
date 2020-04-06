module IR.Builtin where

import IR.IR

base :: Module
base = Module 
  { mid = 0
  , mdata = undefined
  , mcomb = undefined
  }

int :: Name
int = Name 0 0 0