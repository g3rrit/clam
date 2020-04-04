module IR.Convert where

import qualified Parser.AST as AST
import qualified IR.IR as IR
import qualified Data.Map.Strict as M
import Data.Either
import Error.Print

data ConvertError 
  = ConvertError String Loc

convert :: Integer -> AST.Module -> Either IR.Module ConvertError
convert id m = Left $ IR.Module 
  { IR.mid   = id
  , IR.mdata = M.fromList $ map (\d -> (IR.dname d, d)) ds
  , IR.mcomb = M.fromList $ map (\c -> (IR.cname c, c)) cs
  }
  where 
    (cs, ds) = let (cs', ds') = partitionEithers $ AST.ms m in (map convertComb cs', map convertData ds')

-- TODO

convertData :: AST.Data -> IR.Data
convertData _ = IR.Data (5, 5, 5) [IR.Variant (1,1,1) []]

convertComb :: AST.Comb -> IR.Comb
convertComb _ = IR.Comb (2,2,2) (IR.EVar (4,4,4,4) (IR.TPrim (3,3,3))) 
