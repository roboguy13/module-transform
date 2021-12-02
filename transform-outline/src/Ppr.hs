module Ppr
  where

import           Data.List

class Ppr a where
  ppr :: a -> String

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

