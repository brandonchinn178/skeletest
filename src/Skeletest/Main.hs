{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

import Control.Monad (when)
import Data.Text.IO qualified as Text
import Skeletest.Internal.CLI (Flag, flag, loadCliArgs)
import Skeletest.Internal.Capture (captureOutputPlugin)
import Skeletest.Internal.Exit (TestExitCode (..), exitWith, handleUnknownErrors)
import Skeletest.Internal.Snapshot (
  SnapshotRenderer (..),
  renderWithShow,
  setSnapshotRenderers,
  snapshotPlugin,
 )
import Skeletest.Internal.Spec (
  Spec,
  SpecInfo (..),
  newSpecRunner,
  specTreePlugin,
 )
import Skeletest.Internal.Spec.Tree (getSpecTests)
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Plugin (Hooks (..), Plugin (..))
import Skeletest.Prop.Internal (propPlugin)
import System.IO qualified as IO

runSkeletest :: [Plugin] -> [(FilePath, Spec)] -> IO ()
runSkeletest userPlugins testModules = handleUnknownErrors $ do
  selections <- loadCliArgs builtinFlags cliFlags
  setSnapshotRenderers snapshotRenderers

  let initialSpecs = map mkSpec testModules
  specs <- hooks.modifySpecRegistry selections pure initialSpecs
  when (null $ concatMap (getSpecTests . (.specSpec)) specs) $ do
    Text.hPutStrLn IO.stderr $ Color.red "ERROR: No tests selected!"
    exitWith ExitNoTests

  runner <- newSpecRunner hooks initialSpecs
  exitCode <- hooks.runSpecs runner.run specs
  runner.printSummary
  exitWith exitCode
 where
  builtinPlugins =
    [ specTreePlugin
    , snapshotPlugin
    , captureOutputPlugin
    , propPlugin
    ]

  hooks = foldMap (.hooks) $ builtinPlugins <> userPlugins
  snapshotRenderers = foldMap (.snapshotRenderers) $ builtinPlugins <> userPlugins

  cliFlags = foldMap (.cliFlags) userPlugins
  builtinFlags = foldMap (.cliFlags) builtinPlugins

  mkSpec (specPath, specSpec) =
    SpecInfo
      { specPath
      , specSpec
      }
