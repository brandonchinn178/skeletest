{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Preprocessor (
  processFile,
  Options (..),
  defaultOptions,
  decodeOptions,
) where

import Control.Monad (guard)
import Data.Char (isDigit, isLower, isUpper)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (SkeletestError (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))
import Text.Read (readMaybe)
import UnliftIO.Exception (throwIO)

data Options = Options
  { mainModuleName :: Text
  , mainFuncName :: Text
  }
  deriving (Show, Read)

defaultOptions :: Options
defaultOptions =
  Options
    { mainModuleName = "Main"
    , mainFuncName = "main"
    }

encodeOptions :: Options -> Text
encodeOptions = Text.pack . show

decodeOptions :: Text -> Either Text Options
decodeOptions =
  maybe (Left "Could not decode skeletest-preprocessor options") Right
    . readMaybe
    . Text.unpack
    . unquote
 where
  unquote s =
    case Text.stripPrefix "\"" s >>= Text.stripSuffix "\"" of
      Just s' -> Text.replace "\\\"" "\"" s'
      Nothing -> s

-- | Preprocess the given Haskell file. See Main.hs
processFile :: Options -> FilePath -> Text -> IO Text
processFile options path file = do
  file' <-
    if getModuleName file == options.mainModuleName
      then updateMainFile path file
      else pure file
  pure
    . addLine pluginPragma
    . addLine linePragma
    $ file'
 where
  addLine line f = line <> "\n" <> f
  quoted s = "\"" <> s <> "\""

  pluginMod = "Skeletest.Internal.Plugin"
  quote s = "\"" <> Text.replace "\"" "\\\"" s <> "\""
  pluginPragma =
    Text.unwords
      [ "{-# OPTIONS_GHC"
      , "-fplugin=" <> pluginMod
      , "-fplugin-opt=" <> pluginMod <> ":" <> (quote . encodeOptions) options
      , "#-}"
      ]
  linePragma =
    -- this is needed to tell GHC to use original path in error messages
    "{-# LINE 1 " <> quoted (Text.pack path) <> " #-}"

getModuleName :: Text -> Text
getModuleName file =
  case mapMaybe parseModuleLine $ Text.lines file of
    -- there was a module line
    [name] -> name
    -- there were no module lines, it's the Main module
    [] -> "Main"
    -- something else? just silently ignore it
    _ -> ""
 where
  parseModuleLine s =
    case Text.words s of
      "module" : name : _ -> Just name
      _ -> Nothing

updateMainFile :: FilePath -> Text -> IO Text
updateMainFile path file = do
  modules <- findTestModules path
  either throwIO pure $
    pure file
      >>= insertImports modules
      >>= pure . addSpecsList modules

-- | Find all test modules using the given path to the Main module.
--
-- >>> findTestModules "test/Main.hs"
-- ["My.Module.Test1", "My.Module.Test2", ...]
findTestModules :: FilePath -> IO [(FilePath, Text)]
findTestModules path = mapMaybe toTestModule <$> listDirectoryRecursive testDir
 where
  testDir = takeDirectory path

  toTestModule fp = do
    guard (fp /= path)
    (fpNoExt, ".hs") <- pure $ splitExtensions fp
    guard ("Spec" `Text.isSuffixOf` Text.pack fpNoExt)
    name <- moduleNameFromPath $ Text.pack $ makeRelative testDir fpNoExt
    pure (fp, name)

  moduleNameFromPath = fmap (Text.intercalate ".") . mapM validateModuleName . Text.splitOn "/"

  -- https://www.haskell.org/onlinereport/syntax-iso.html
  -- large { small | large | digit | ' }
  validateModuleName name = do
    (first, rest) <- Text.uncons name
    guard $ isUpper first
    guard $ Text.all (\c -> isUpper c || isLower c || isDigit c || c == '\'') rest
    pure name

addSpecsList :: [(FilePath, Text)] -> Text -> Text
addSpecsList testModules file =
  Text.unlines
    [ file
    , mainFileSpecsListIdentifier <> " :: [(FilePath, Spec)]"
    , mainFileSpecsListIdentifier <> " = " <> renderSpecList specsList
    ]
 where
  specsList =
    [ (quote $ Text.pack fp, modName <> ".spec")
    | (fp, modName) <- testModules
    ]
  quote s = "\"" <> s <> "\""
  renderSpecList xs = "[" <> (Text.intercalate ", " . map renderSpecInfo) xs <> "]"
  renderSpecInfo (fp, spec) = "(" <> fp <> ", " <> spec <> ")"

-- | Add imports after the Skeletest.Main import, which should always be present in the Main module.
insertImports :: [(FilePath, Text)] -> Text -> Either SkeletestError Text
insertImports testModules file =
  let (pre, post) = break isSkeletestImport $ Text.lines file
   in if null post
        then Left $ CompilationError Nothing "Could not find Skeletest.Main import in Main module"
        else pure . Text.unlines $ pre <> importTests <> post
 where
  isSkeletestImport line =
    case Text.words line of
      "import" : "Skeletest.Main" : _ -> True
      _ -> False

  importTests =
    [ "import qualified " <> name
    | (_, name) <- testModules
    ]

{----- Helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap (sort . concat) . mapM (go . (fp </>)) =<< listDirectory fp
 where
  go child = do
    isDir <- doesDirectoryExist child
    if isDir
      then listDirectoryRecursive child
      else pure [child]
