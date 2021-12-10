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

import           Control.Monad.State

import           JavaParser
import           Parser (runParser)

import           Ppr

import Debug.Trace

data ImportGraph = ImportGraph { getImportGraphEdges :: Set (Name, Name) }
  deriving (Show)

moduleNameToPath :: Name -> Maybe String
moduleNameToPath (Name idents)
  | ("*":_) <- reverse idents = Nothing  -- NOTE: Ignore '*' imports for now
  | otherwise                 = Just . (++".java") . intercalate "/" $ idents

removeJavaExt :: String -> String
removeJavaExt = composeList $ replicate javaExtLen init
  where
    javaExtLen = length ".java"

composeList :: [a -> a] -> a -> a
composeList = foldr (.) id

pathToName :: String -> Name
pathToName = Name . splitOn "/" . removeJavaExt

readModName :: String -> Name
readModName = Name . splitOn "."

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
getImportGraph basePaths = flip evalStateT mempty . getImportGraph' basePaths

getImportGraph' :: [String] -> Name -> StateT (Set Name) IO ImportGraph
getImportGraph' basePaths topName
  | Just fileName <- moduleNameToPath topName = do
  visited <- get
  if topName `Set.member` visited
    then pure $ ImportGraph mempty
    else
      lift (readFileWithPaths basePaths fileName) >>= \case
        Nothing -> pure $ ImportGraph mempty
        Just contents -> -- trace ("module " ++ show topName) $
          case runParser parseJava contents of
            Nothing -> error $ "Parse error in " ++ fileName

            Just (str0, Module _pkgDecl importDecls) -> do
              -- traceM $ "importDecls = " ++ show importDecls
              modify (Set.insert topName)
              -- insert topName
              let currEdges = map (mkImportEdge topName) $ filter (not . hasWildcard) importDecls
                  importNames = Set.fromList $ map importName importDecls

              -- ImportGraph rest <- fmap joinImportGraphs $ mapM (\mod -> getImportGraph' (visited `Set.union` (Set.delete mod importNames)) basePaths mod) (Set.toList (importNames `Set.difference` visited))
              ImportGraph rest <- fmap joinImportGraphs $ mapM (getImportGraph' basePaths) $ Set.toList importNames

              pure (ImportGraph (Set.fromList currEdges <> rest))
              -- joinImportGraphs <$> fmap catMaybes (traverse (go topName) importDecls)

  where
    go currName (Import _ modName)
      -- | currName `elem` visited = trace ("already visited " ++ show modName) $ pure Nothing
      | otherwise =
          case moduleNameToPath modName of
            Nothing -> trace ("Not found: " ++ ppr modName) $ pure Nothing
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
getImportGraph' _ _ = return $ ImportGraph mempty

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

    (initialModule:basePaths@(_:_)) -> do
      tree <- go basePaths (readModName initialModule)
      putStrLn $ genDOT tree
      -- print tree

    args -> error $ "Expected at least 2 arguments. Got " ++ show (length args)

  where
    go = getImportGraph

