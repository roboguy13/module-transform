{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import           Language.Java.Parser
import           Language.Java.Syntax
import           Language.Java.Pretty

import           Data.List
import           Data.List.Split

import           Data.Maybe

import           System.Environment
import           System.FilePath
import           System.Directory

data ImportTree = Node [(Name, ImportTree)]
  deriving (Show)

getIdent :: Ident -> String
getIdent (Ident str) = str

moduleNameToPath :: Name -> Maybe String
moduleNameToPath (Name idents)
  | (Ident "*":_) <- reverse idents = Nothing  -- NOTE: Ignore '*' imports for now
  | otherwise                       = Just . (++".java") . intercalate "/" . map getIdent $ idents

removeJavaExt :: String -> String
removeJavaExt = composeList $ replicate javaExtLen init
  where
    javaExtLen = length ".java"

composeList :: [a -> a] -> a -> a
composeList = foldr (.) id

pathToName :: String -> Name
pathToName = Name . map Ident . splitOn "/" . removeJavaExt

importDeclModuleName :: ImportDecl -> Name
importDeclModuleName (ImportDecl _ modName _) = modName

getImportTree :: String -> String -> IO ImportTree
getImportTree basePath fileName = do
  contents <- readFile fileName

  case parser compilationUnit contents of
    Left err -> error $ "Parse error:\n" ++ show err

    Right (CompilationUnit _pkgDecl importDecls _typeDecls) ->
      Node <$> fmap catMaybes (traverse go importDecls)

  where
    go (ImportDecl _ modName _) =
      case moduleNameToPath modName of
        Nothing -> pure Nothing
        Just path -> do
          let fullPath = basePath </> path

          doesFileExist fullPath >>= \case
            False -> pure Nothing
            True -> Just <$> (fmap (modName, ) (getImportTree basePath (basePath </> path)))

connect :: Name -> Name -> String
connect name1 name2 = show (prettyPrint name1) <> " -> " <> show (prettyPrint name2) <> ";"

genDOT :: Name -> ImportTree -> String
genDOT topName importTree =
  unlines
    [ "strict digraph {"
    , unlines $ map ("  " ++) $ go topName importTree
    , "}"
    ]
  where
    go currName (Node subtrees) =
      map (connect currName . fst) subtrees
        ++ concatMap (uncurry go) subtrees

main :: IO ()
main = do
  getArgs >>= \case

    [basePath, fileName] -> do
      tree <- go basePath (basePath </> fileName)
      putStrLn $ genDOT (pathToName fileName) tree

    args -> error $ "Expected 2 arguments. Got " ++ show (length args)

  where
    go = getImportTree

