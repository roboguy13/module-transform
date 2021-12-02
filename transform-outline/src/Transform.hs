--
-- Traditional  ==>  PModule
--

module Transform
  where

import           Traditional
import           Parametrized

import           Ppr

import           Name
import           Fun

import           Data.List
import           Data.Char

import           Data.Set (Set)
import qualified Data.Set as Set

-- type Constraints = Set ModuleParam

genModVar :: ModuleName -> Name
genModVar (ModuleName ns) =
  Name $ ("$"++) $ map toLower $ intercalate "_" $ map unName ns

transformModule :: Traditional -> PModule
transformModule (Traditional modName is fns) =
  let params = map (mkParam . getImportModName) is
  in
  PModule
    modName
    params
    (map (fmap genModVar) fns)

mkParam :: ModuleName -> ModuleParam
mkParam modName = ModuleParam (genModVar modName) modName

