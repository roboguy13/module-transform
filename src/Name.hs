module Name where

import           Data.List
import           Ppr

newtype Name = Name { unName :: String }

newtype ModuleName = ModuleName { unModuleName :: [Name] }

instance Ppr Name where
  ppr (Name str) = str

instance Ppr ModuleName where
  ppr (ModuleName path) = intercalate "." $ map ppr path

