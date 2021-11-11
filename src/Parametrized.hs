--
-- Parametrized modules
--

module Parametrized
  where

import           Fun
import           Name

import           Ppr

import           Stmt
import           Expr

import           Data.List

-- | @ModuleParam "io" "IO"@  ===  @io : IO@
data ModuleParam = ModuleParam Name ModuleName

data PModule =
  PModule ModuleName [ModuleParam] [Fun Name]

instance Ppr ModuleParam where
  ppr (ModuleParam ident mod) = ppr ident ++ " : " ++ ppr mod

instance Ppr PModule where
  ppr (PModule modName modParams funs) =
    "module " ++ ppr modName ++ "(" ++ paramList ++ ") {\n" ++ pprFuns funs ++ "}"
    where
      paramList = intercalate ", " $ map ppr modParams

example :: PModule
example =
  PModule
    (ModuleName [Name "m"])
    [ModuleParam (Name "io") ioModName]
    [Fun (Name "f") []
      (Assign (Name "_") (moduleCall (Name "io") (Name "println") []))
    ]

