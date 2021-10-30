module Fun
  where

import           Name
import           Stmt
import           Ppr

import           Data.List

data Fun a = Fun Name [Name] (Stmt a)

instance Ppr a => Ppr (Fun a) where
  ppr (Fun n params body) = "fun " ++ ppr n ++ "(" ++ paramList ++ ") {\n" ++ indent (ppr body) ++ "\n}"
    where
      paramList = intercalate "," $ map ppr params

pprFuns :: Ppr a => [Fun a] -> String
pprFuns funs = unlines ((map (indent . ppr) funs))

