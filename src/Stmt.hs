module Stmt
  where

import           Expr
import           Name
import           Ppr

data Stmt a
  = Assign Name (Expr a)
  | Seq (Stmt a) (Stmt a)
  | While (Expr a) (Stmt a)
  | ExprStmt (Expr a)
  | Return (Expr a)

instance Ppr a => Ppr (Stmt a) where
  ppr (Assign n e) = ppr n ++ " = " ++ ppr e ++ ";"
  ppr (Seq x y) = ppr x ++ "\n" ++ ppr y
  ppr (While e body) = "while (" ++ ppr e ++ ") {\n" ++ indent (ppr body) ++ "\n}"
  ppr (ExprStmt e) = ppr e ++ ";"
  ppr (Return e) = "return " ++ ppr e ++ ";"

