{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Preprocessor (
  processFile,
  Options (..),
  defaultOptions,
  decodeOptions,
) where

import Control.Monad (guard, when, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict qualified as State
import Data.Char (isDigit, isLower, isUpper)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (SkeletestError (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, splitExtensions, takeDirectory, (</>))
import Text.Read (readMaybe)
import UnliftIO.Exception (fromEither)

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
updateMainFile mainFile file = do
  let testDir = takeDirectory mainFile
  allFiles <- filter (/= mainFile) <$> listDirectoryRecursive testDir
  let modules = mapMaybe (toTestModule testDir) allFiles
  fromEither $
    runMainFileTransformer
      ( addSpecsList modules
          >=> insertImports -- Must be last!
      )
      file

-- | Find all test modules using the given path to the Main module.
--
-- >>> findTestModules "test/Main.hs"
-- ["My.Module.Test1", "My.Module.Test2", ...]
toTestModule :: FilePath -> FilePath -> Maybe (FilePath, Text)
toTestModule testDir fp = do
  (fpNoExt, ".hs") <- pure $ splitExtensions fp
  guard ("Spec" `Text.isSuffixOf` Text.pack fpNoExt)
  name <- moduleNameFromPath . Text.pack . makeRelative testDir $ fpNoExt
  pure (fp, name)
 where
  moduleNameFromPath = fmap (Text.intercalate ".") . mapM validateModuleName . Text.splitOn "/"

  -- https://www.haskell.org/onlinereport/syntax-iso.html
  -- large { small | large | digit | ' }
  validateModuleName name = do
    (first, rest) <- Text.uncons name
    guard $ isUpper first
    guard $ Text.all (\c -> isUpper c || isLower c || isDigit c || c == '\'') rest
    pure name

{----- Main file generation -----}

type ImportDef = (Text, Text) -- (module name, qualified as)
type MainFileTransformerM a = State.StateT [ImportDef] (Except.ExceptT SkeletestError Identity) a
type MainFileTransformer = Text -> MainFileTransformerM Text

runMainFileTransformer :: MainFileTransformer -> Text -> Either SkeletestError Text
runMainFileTransformer transform =
  runIdentity
    . Except.runExceptT
    . (`State.evalStateT` [])
    . transform

addImport :: ImportDef -> MainFileTransformerM ()
addImport i = State.modify (i :)

addSpecsList :: [(FilePath, Text)] -> MainFileTransformer
addSpecsList testModules file = do
  specsList <- mapM mkSpecDef testModules
  pure . Text.unlines $
    [ file
    , mainFileSpecsListIdentifier <> " :: [(FilePath, Spec)]"
    , mainFileSpecsListIdentifier <> " = " <> (renderList . map renderPair) specsList
    ]
 where
  mkSpecDef (fp, modName) = do
    addImport (modName, modName)
    pure (quote $ Text.pack fp, modName <> ".spec")
  quote s = "\"" <> s <> "\""
  renderList xs = "[" <> Text.intercalate ", " xs <> "]"
  renderPair (x, y) = "(" <> x <> ", " <> y <> ")"

-- | Add imports after the Skeletest.Main import, which should always be present in the Main module.
insertImports :: MainFileTransformer
insertImports file = do
  imports <- State.get

  let (pre, post) = break isSkeletestImport $ Text.lines file
  when (null post) $ do
    lift . Except.throwE $
      CompilationError Nothing "Could not find Skeletest.Main import in Main module"

  pure . Text.unlines $ pre <> map mkImport imports <> post
 where
  isSkeletestImport line =
    case Text.words line of
      "import" : "Skeletest.Main" : _ -> True
      _ -> False
  mkImport (name, alias) = "import qualified " <> name <> " as " <> alias

{----- Helpers -----}

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive fp = fmap (sort . concat) . mapM (go . (fp </>)) =<< listDirectory fp
 where
  go child = do
    isDir <- doesDirectoryExist child
    if isDir
      then listDirectoryRecursive child
      else pure [child]
