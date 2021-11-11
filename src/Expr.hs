{-# LANGUAGE DeriveFunctor #-}

module Expr
  where

import           Name
import           Ppr

import           Data.List

data Expr a
  = Var Name
  | Lit Int
  | Sub (Expr a) (Expr a)
  | Call (FnCallName a) [Expr a]
  | ModuleMember a Name  -- | @ModuleName "M" "x"@  ===  @M.x@
  deriving (Functor)

data FnCallName a = FnCallLocal Name | FnCallModule a Name
  deriving (Functor)

moduleCall :: a -> Name -> [Expr a] -> Expr a
moduleCall modName fnName = Call (FnCallModule modName fnName)

instance Ppr a => Ppr (FnCallName a) where
  ppr (FnCallLocal n) = ppr n
  ppr (FnCallModule mod n) = ppr mod ++ "." ++ ppr n

instance Ppr a => Ppr (Expr a) where
  ppr (Var v) = ppr v
  ppr (Lit i) = show i
  ppr (Sub x y) = '(' : ppr x ++ ") - (" ++ ppr y ++ ")"
  ppr (Call n args) = ppr n ++ "(" ++ intercalate ", " (map ppr args) ++ ")"
  ppr (ModuleMember mod n) = ppr mod ++ "." ++ ppr n

