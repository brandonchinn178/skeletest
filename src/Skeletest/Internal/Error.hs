{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Error (
  SkeletestError (..),
  skeletestPluginError,
  invariantViolation,
) where

import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Utils.Panic (pgmError)
import UnliftIO.Exception (Exception (..))

data SkeletestError
  = TestInfoNotFound
  | CliFlagNotFound Text
  | FixtureCircularDependency [Text]
  | SnapshotFileCorrupted FilePath
  deriving (Show)

instance Exception SkeletestError where
  displayException = Text.unpack . \case
    TestInfoNotFound ->
      "Could not find test info"
    CliFlagNotFound name ->
      "CLI flag '" <> name <> "' was not registered. Did you add it to cliFlags in Main.hs?"
    FixtureCircularDependency fixtures ->
      "Found circular dependency when resolving fixtures: " <> Text.intercalate " -> " fixtures
    SnapshotFileCorrupted fp ->
      "Snapshot file was corrupted: " <> Text.pack fp

-- | Throw a user error during compilation, e.g. during the preprocessor or plugin phases.
skeletestPluginError :: String -> a
skeletestPluginError msg =
  pgmError . dropWhileEnd (== '\n') . unlines $
    [ ""
    , "******************** skeletest failure ********************"
    , msg
    ]

-- | Throw an error in a situation that should never happen, and indicates a bug.
invariantViolation :: String -> a
invariantViolation msg =
  error . unlines $
    [ "Invariant violation: " <> msg
    , "**** This is a skeletest bug. Please report it at https://github.com/brandonchinn178/skeletest/issues"
    ]
