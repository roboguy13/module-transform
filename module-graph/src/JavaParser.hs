module JavaParser where

import           Data.List
import           Data.Maybe

import           Ppr
import           Parser

import           Control.Applicative
import           Data.Functor

data Name = Name { unName :: [String] }
  deriving (Show, Eq, Ord)

instance Ppr Name where
  ppr (Name idents) = intercalate "." idents

data Module =
  Module
    { modName :: Maybe Name
    , modImports :: [Import]
    }
  deriving (Show)

data Import =
  Import
    { importStatic :: Bool
    , importName :: Name
    }
  deriving (Show)

getImportWildcard :: Import -> Maybe Name
getImportWildcard im
  | let unnamed = unName (importName im)
  , ("*":_) <- reverse (unName (importName im)) =
      Just $ Name (init unnamed)

  | otherwise = Nothing

hasWildcard :: Import -> Bool
hasWildcard = isJust . getImportWildcard

parseJava :: Parser Module
parseJava = do
  many parseComment
  many parseSpace'
  pkgName <- optional parsePackageDecl

  many parseSpace'

  ims <- concat . maybeToList <$> optional parseImports

  pure (Module pkgName ims)

parsePackageDecl :: Parser Name
parsePackageDecl = parseString "package" *> some parseSpace *> parseName <* many parseSpace <* parseChar ';'

parseImports :: Parser [Import]
parseImports = do
  im <- parseImport
  rest <- many (many parseSpace' *> parseImport)
  pure (im:rest)

parseImport :: Parser Import
parseImport = do
  parseString "import"
  some parseSpace'

  staticKeyword <- optional (parseString "static" <* some parseSpace')

  name <- parseName

  many parseSpace'
  parseChar ';'

  pure (Import (isJust staticKeyword) name)

parseName :: Parser Name
parseName = do
  ident <- parseIdent
  rest <- many (parseChar '.' *> parseIdent)
  pure (Name (ident:rest))

-- NOTE: This also recognizes integer literals
parseIdent :: Parser String
parseIdent = some parseAlphanum <|> parseString "*"

parseLineComment :: Parser ()
parseLineComment = parseString "//" *> many (parseWhen (not . (`elem` "\n\r"))) *> parseNewline *> pure ()

parseBlockComment :: Parser ()
parseBlockComment = parseString "/*" *> parseNotString "*/" *> parseString "*/" *> pure ()

parseSpace' :: Parser ()
parseSpace' = void (many parseSpace *> parseComment *> many parseSpace) <|> void parseSpace

parseComment :: Parser ()
parseComment = parseLineComment <|> parseBlockComment

