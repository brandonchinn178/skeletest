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
import Skeletest.Internal.Capture (CaptureOutputFlag)
import Skeletest.Internal.Exit (TestExitCode (..), exitWith, handleUnknownErrors)
import Skeletest.Internal.Snapshot (
  SnapshotRenderer (..),
  SnapshotUpdateFlag,
  defaultSnapshotRenderers,
  renderWithShow,
  setSnapshotRenderers,
  snapshotsHook,
 )
import Skeletest.Internal.Spec (
  Spec,
  SpecInfo (..),
  applyTestSelectionsHook,
  focusHook,
  manualTestsHook,
  runSpecs,
  skipHook,
  xfailHook,
 )
import Skeletest.Internal.Spec.Tree (getSpecTests)
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Plugin (Hooks (..), Plugin (..))
import Skeletest.Prop.Internal (PropLimitFlag, PropSeedFlag)
import System.IO qualified as IO

runSkeletest :: [Plugin] -> [(FilePath, Spec)] -> IO ()
runSkeletest = runSkeletest' . mconcat

runSkeletest' :: Plugin -> [(FilePath, Spec)] -> IO ()
runSkeletest' Plugin{hooks = hooks0, ..} testModules = handleUnknownErrors $ do
  selections <- loadCliArgs builtinFlags cliFlags
  setSnapshotRenderers (snapshotRenderers <> defaultSnapshotRenderers)

  let initialSpecs = map mkSpec testModules
  specs <- hooks.modifySpecRegistry selections pure initialSpecs
  when (null $ concatMap (getSpecTests . (.specSpec)) specs) $ do
    Text.hPutStrLn IO.stderr $ Color.red "ERROR: No tests selected!"
    exitWith ExitNoTests

  success <- runSpecs hooks specs
  exitWith $ if success then ExitSuccess else ExitTestFailure
 where
  hooks = mconcat builtinHooks <> hooks0

  builtinHooks =
    [ xfailHook
    , skipHook
    , focusHook
    , applyTestSelectionsHook
    , manualTestsHook
    , snapshotsHook
    ]

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
