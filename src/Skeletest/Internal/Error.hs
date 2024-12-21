{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Error (
  SkeletestError (..),
  skeletestPluginError,
  invariantViolation,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Utils.Panic (pgmError)
import UnliftIO.Exception (Exception (..), impureThrow)

data SkeletestError
  = -- | A user error during compilation, e.g. during the preprocessor or plugin phases.
    CompilationError Text
  | -- | An error in a situation that should never happen, and indicates a bug.
    InvariantViolation Text
  | TestInfoNotFound
  | CliFlagNotFound Text
  | FixtureCircularDependency [Text]
  | SnapshotFileCorrupted FilePath
  deriving (Show)

instance Exception SkeletestError where
  displayException =
    Text.unpack . \case
      CompilationError msg ->
        Text.unlines
          [ ""
          , "******************** skeletest failure ********************"
          , msg
          ]
      InvariantViolation msg ->
        Text.unlines
          [ "Invariant violation: " <> msg
          , "**** This is a skeletest bug. Please report it at https://github.com/brandonchinn178/skeletest/issues"
          ]
      TestInfoNotFound ->
        "Could not find test info"
      CliFlagNotFound name ->
        "CLI flag '" <> name <> "' was not registered. Did you add it to cliFlags in Main.hs?"
      FixtureCircularDependency fixtures ->
        "Found circular dependency when resolving fixtures: " <> Text.intercalate " -> " fixtures
      SnapshotFileCorrupted fp ->
        "Snapshot file was corrupted: " <> Text.pack fp

skeletestPluginError :: String -> a
skeletestPluginError = pgmError . stripEnd . displayException . CompilationError . Text.pack
  where
    stripEnd = Text.unpack . Text.stripEnd . Text.pack

invariantViolation :: String -> a
invariantViolation = impureThrow . InvariantViolation . Text.pack
