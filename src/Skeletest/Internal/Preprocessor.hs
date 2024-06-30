{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Preprocessor (
  processFile,
) where

import Control.Monad (guard)
import Data.Char (isDigit, isLower, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))

import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (skeletestPluginError)

-- | Preprocess the given Haskell file. See Main.hs
processFile :: FilePath -> Text -> IO Text
processFile path file = do
  file' <- if isMain file then updateMainFile path file else pure file
  pure
    . addLine pluginPragma
    . addLine linePragma
    $ file'
  where
    addLine line f = line <> "\n" <> f
    quoted s = "\"" <> s <> "\""

    pluginPragma = "{-# OPTIONS_GHC -fplugin=Skeletest.Internal.Plugin #-}"
    linePragma =
      -- this is needed to tell GHC to use original path in error messages
      "{-# LINE 1 " <> quoted (Text.pack path) <> " #-}"

isMain :: Text -> Bool
isMain file =
  case mapMaybe getModuleName $ Text.lines file of
    -- there was a module line
    [name] -> name == "Main"
    -- there were no module lines, it's the main module
    [] -> True
    -- something else? just silently ignore it
    _ -> False
  where
    getModuleName s =
      case Text.words s of
        "module" : name : _ -> Just name
        _ -> Nothing

updateMainFile :: FilePath -> Text -> IO Text
updateMainFile path file = do
  modules <- findTestModules path
  pure
    . addSpecsList modules
    . insertImports modules
    $ file

-- | Find all test modules using the given path to the Main module.
--
-- >>> findTestModules "test/Main.hs"
-- ["My.Module.Test1", "My.Module.Test2", ...]
findTestModules :: FilePath -> IO [Text]
findTestModules path = mapMaybe toTestModule <$> listDirectoryRecursive testDir
  where
    testDir = takeDirectory path

    toTestModule fp = do
      guard (fp /= path)
      (fpNoExt, ".hs") <- pure $ splitExtensions fp
      moduleNameFromPath $ Text.pack $ makeRelative testDir fpNoExt

    moduleNameFromPath = fmap (Text.intercalate ".") . mapM validateModuleName . Text.splitOn "/"

    -- https://www.haskell.org/onlinereport/syntax-iso.html
    -- large { small | large | digit | ' }
    validateModuleName name = do
      (first, rest) <- Text.uncons name
      guard $ isUpper first
      guard $ Text.all (\c -> isUpper c || isLower c || isDigit c || c == '\'') rest
      Just name

addSpecsList :: [Text] -> Text -> Text
addSpecsList moduleNames file =
  Text.unlines
    [ file
    , mainFileSpecsListIdentifier <> " :: [(String, Spec)]"
    , mainFileSpecsListIdentifier <> " = " <> renderList specsList
    ]
  where
    specsList =
      [ (Text.pack (show name), name <> ".spec")
      | name <- moduleNames
      ]
    renderList xs = "[" <> (Text.intercalate ", " . map renderPair) xs <> "]"
    renderPair (a, b) = "(" <> a <> ", " <> b <> ")"

-- | Add imports after the Skeletest.Main import, which should always be present in the Main module.
--
-- TODO: handle user using explicit multiline import list in Skeletest.Main import
insertImports :: [Text] -> Text -> Text
insertImports moduleNames file =
  let (pre, post) = break isSkeletestImport $ Text.lines file
   in if null post
        then skeletestPluginError "Could not find Skeletest.Main import in Main module"
        else Text.unlines $ pre <> importTests <> post
  where
    isSkeletestImport line =
      case Text.words line of
        "import" : "Skeletest.Main" : _ -> True
        _ -> False

    importTests =
      [ "import qualified " <> name
      | name <- moduleNames
      ]

{----- Helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap concat . mapM (go . (fp </>)) =<< listDirectory fp
  where
    go child = do
      isDir <- doesDirectoryExist child
      if isDir
        then listDirectoryRecursive child
        else pure [child]
