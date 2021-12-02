{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- import           Language.Java.Parser
-- import           Language.Java.Syntax
-- import           Language.Java.Pretty

import           Data.List
import           Data.List.Split

import           Data.Maybe

import           System.Environment
import           System.FilePath
import           System.Directory

import           Data.Set (Set)
import qualified Data.Set as Set

import           JavaParser
import           Parser (runParser)

import           Ppr

import Debug.Trace

data ImportGraph = ImportGraph { getImportGraphEdges :: Set (Name, Name) }
  deriving (Show)

moduleNameToPath :: Name -> Maybe String
moduleNameToPath (Name idents)
  | ("*":_) <- reverse idents = Nothing  -- NOTE: Ignore '*' imports for now
  | otherwise                       = Just . (++".java") . intercalate "/" $ idents

removeJavaExt :: String -> String
removeJavaExt = composeList $ replicate javaExtLen init
  where
    javaExtLen = length ".java"

composeList :: [a -> a] -> a -> a
composeList = foldr (.) id

pathToName :: String -> Name
pathToName = Name . splitOn "/" . removeJavaExt

importDeclModuleName :: Import -> Name
importDeclModuleName (Import _ modName) = modName

doesFileExistWithPaths :: [String] -> String -> IO Bool
doesFileExistWithPaths basePaths fileName =
  fmap or $ mapM doesFileExist (map (</> fileName) ("":basePaths))

readFileWithPaths :: [String] -> String -> IO (Maybe String)
readFileWithPaths paths0 = go ("":paths0)
  where
    go [] fileName = pure Nothing -- error $ "Cannot find file: " ++ fileName
    go (path:paths) fileName =
      let fullPath = path </> fileName
      in
      doesFileExist fullPath >>= \case
        True -> Just <$> readFile fullPath
        False -> go paths fileName

insertEdge :: (Name, Name) -> ImportGraph -> ImportGraph
insertEdge edge (ImportGraph graph) = ImportGraph (Set.insert edge graph)

joinImportGraphs :: [ImportGraph] -> ImportGraph
joinImportGraphs = ImportGraph . foldr Set.union mempty . map getImportGraphEdges

mkImportEdge :: Name -> Import -> (Name, Name)
mkImportEdge name1 (Import _ name2) = (name1, name2)

getImportGraph :: [String] -> Name -> IO ImportGraph
getImportGraph = getImportGraph' mempty

getImportGraph' :: Set Name -> [String] -> Name -> IO ImportGraph
getImportGraph' visited basePaths topName
  | topName `Set.member` visited = pure $ ImportGraph mempty

  | Just fileName <- moduleNameToPath topName = do
  readFileWithPaths basePaths fileName >>= \case
    Nothing -> pure $ ImportGraph mempty
    Just contents -> trace ("module " ++ show topName) $
      case runParser parseJava contents of
        Nothing -> error $ "Parse error in " ++ fileName

        Just (_, Module _pkgDecl importDecls) -> do
          let currEdges = map (mkImportEdge topName) $ filter (not . hasWildcard) importDecls
              importNames = Set.fromList $ map importName importDecls
          ImportGraph rest <- fmap joinImportGraphs $ mapM (getImportGraph' (visited `Set.union` importNames) basePaths) (Set.toList (importNames `Set.difference` visited))
          pure (ImportGraph (Set.fromList currEdges <> rest))
          -- joinImportGraphs <$> fmap catMaybes (traverse (go topName) importDecls)

  where
    go currName (Import _ modName)
      -- | currName `elem` visited = trace ("already visited " ++ show modName) $ pure Nothing
      | otherwise =
          case moduleNameToPath modName of
            Nothing -> pure Nothing
            Just path -> do
              -- let fullPath = basePath </> path

              doesFileExistWithPaths basePaths path >>= \case
                False -> pure Nothing
                True ->
                  pure (Just (currName, modName))
                  -- let currEdge = (currName, modName)
                  -- in
                  -- Just <$> (insertEdge currEdge <$> (getImportGraph' (currName : visited) modName basePaths path))
                -- True -> Just <$> (fmap (modName, ) (getImportGraph' (modName : visited) basePaths path))
getImportGraph' _ _ _ = pure $ ImportGraph mempty

connect :: Name -> Name -> String
connect name1 name2 = show (ppr name1) <> " -> " <> show (ppr name2) <> ";"

genDOT :: ImportGraph -> String
genDOT importTree =
  unlines
    [ "strict digraph {"
    , unlines $ map ("  " ++) $ go importTree
    , "}"
    ]
  where
    go (ImportGraph edges) =
      map (uncurry connect) $ Set.toList edges
      -- map (connect currName . fst) subtrees
      --   ++ concatMap (uncurry go) subtrees

main :: IO ()
main = do
  getArgs >>= \case

    (fileName:basePaths@(_:_)) -> do
      tree <- go basePaths (pathToName fileName)
      putStrLn $ genDOT tree
      -- print tree

    args -> error $ "Expected at least 2 arguments. Got " ++ show (length args)

  where
    go = getImportGraph

