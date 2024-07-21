{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Main (
  runSkeletest,

  -- * CLI flags
  Flag,
  flag,

  -- * Snapshots
  SnapshotRenderer (..),
  renderWithShow,

  -- * Plugins
  Plugin,

  -- * Re-exports
  Spec,
) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Exit (exitFailure)

import Skeletest.Internal.CLI (Flag, flag, loadCliArgs)
import Skeletest.Internal.Snapshot (
  SnapshotRenderer (..),
  SnapshotUpdateFlag,
  defaultSnapshotRenderers,
  renderWithShow,
  setSnapshotRenderers,
 )
import Skeletest.Internal.Spec (
  Spec,
  SpecInfo (..),
  applyTestSelections,
  pruneSpec,
  runSpecs,
 )
import Skeletest.Plugin (Plugin (..))

runSkeletest :: [Plugin] -> [(FilePath, String, Spec)] -> IO ()
runSkeletest = runSkeletest' . mconcat

runSkeletest' :: Plugin -> [(FilePath, String, Spec)] -> IO ()
runSkeletest' Plugin{..} testModules = do
  selections <- loadCliArgs builtinFlags cliFlags
  setSnapshotRenderers (snapshotRenderers <> defaultSnapshotRenderers)

  let initialSpecs = map mkSpec testModules
  success <- runSpecs . pruneSpec . applyTestSelections selections $ initialSpecs
  unless success exitFailure
  where
    builtinFlags =
      [ flag @SnapshotUpdateFlag
      ]

    mkSpec (specPath, name, specSpec) =
      SpecInfo
        { specPath
        , specName = stripSuffix "Spec" $ Text.pack name
        , specSpec
        }

    -- same as Text.stripSuffix, except return original string if not match
    stripSuffix suf s = fromMaybe s $ Text.stripSuffix suf s
