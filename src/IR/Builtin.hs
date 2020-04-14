module IR.Builtin where

import IR.IR
import IR.Namespace

base :: Module
base = Module 
  { mid = 0
  , mdata = undefined
  , mcomb = undefined
  }

int :: Name
int = Name 0 0 0