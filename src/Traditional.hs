--
-- Non-parametrized modules
--

module Traditional
  where

import           Ppr

import           Name
import           Fun

import           Expr
import           Stmt

newtype Import = Import { getImportModName :: ModuleName }

data Traditional =
  Traditional ModuleName [Import] [Fun ModuleName]

instance Ppr Import where
  ppr (Import mod) = "import " ++ ppr mod ++ ";"

instance Ppr Traditional where
  ppr (Traditional modName imps funs) =
    "module " ++ ppr modName ++ " {\n"
    ++ indent (unlines (map ppr imps))
    ++ "\n\n"
    ++ pprFuns funs
    ++ "}"


tExample :: Traditional
tExample =
  Traditional
    (ModuleName [Name "m"])
    [Import ioModName]
    [Fun (Name "f") []
      (Assign (Name "_") (moduleCall ioModName (Name "println") []))
    ]

