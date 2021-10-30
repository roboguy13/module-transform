module Expr
  where

import           Name
import           Ppr

import           Data.List

data Expr a
  = Var Name
  | Lit Int
  | Sub (Expr a) (Expr a)
  | Call FnCallName [Expr a]
  | ModuleMember a Name  -- | @ModuleName "M" "x"@  ===  @M.x@

data FnCallName = FnCallLocal Name | FnCallModule ModuleName Name

instance Ppr FnCallName where
  ppr (FnCallLocal n) = ppr n
  ppr (FnCallModule mod n) = ppr mod ++ "." ++ ppr n

instance Ppr a => Ppr (Expr a) where
  ppr (Var v) = ppr v
  ppr (Lit i) = show i
  ppr (Sub x y) = '(' : ppr x ++ ") - (" ++ ppr y ++ ")"
  ppr (Call n args) = ppr n ++ "(" ++ intercalate ", " (map ppr args) ++ ")"
  ppr (ModuleMember mod n) = ppr mod ++ "." ++ ppr n

