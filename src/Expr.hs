module Expr
  where

import           Name
import           Ppr

data Expr a
  = Var Name
  | Lit Int
  | Sub (Expr a) (Expr a)
  | ModuleMember a Name  -- | @ModuleName "M" "x"@  ===  @M.x@

instance Ppr a => Ppr (Expr a) where
  ppr (Var v) = ppr v
  ppr (Lit i) = show i
  ppr (Sub x y) = '(' : ppr x ++ ") - (" ++ ppr y ++ ")"
  ppr (ModuleMember mod n) = ppr mod ++ "." ++ ppr n

