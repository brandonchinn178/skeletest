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
import Skeletest.Internal.CLI (Flag, flag, loadCliArgs)
import Skeletest.Internal.Capture (CaptureOutputFlag)
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
  runSpecs,
 )
import Skeletest.Internal.Spec.Tree (
  applyTestSelections,
  pruneSpec,
 )
import Skeletest.Plugin (Plugin (..))
import Skeletest.Prop.Internal (PropLimitFlag, PropSeedFlag)
import System.Exit (exitFailure)

runSkeletest :: [Plugin] -> [(FilePath, Spec)] -> IO ()
runSkeletest = runSkeletest' . mconcat

runSkeletest' :: Plugin -> [(FilePath, Spec)] -> IO ()
runSkeletest' Plugin{..} testModules = do
  selections <- loadCliArgs builtinFlags cliFlags
  setSnapshotRenderers (snapshotRenderers <> defaultSnapshotRenderers)

  let initialSpecs = map mkSpec testModules
  success <- runSpecs hooks . pruneSpec . applyTestSelections selections $ initialSpecs
  unless success exitFailure
 where
  builtinFlags =
    [ flag @SnapshotUpdateFlag
    , flag @PropSeedFlag
    , flag @PropLimitFlag
    , flag @CaptureOutputFlag
    ]

  mkSpec (specPath, specSpec) =
    SpecInfo
      { specPath
      , specSpec
      }
