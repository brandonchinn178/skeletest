{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Foldable (traverse_)
import Skeletest.Internal.CLI (
  ANSIFlag (..),
  Flag,
  FormatFlag,
  flag,
  getFlag,
  loadCliArgs,
 )
import Skeletest.Internal.Capture (CaptureOutputFlag (..), captureOutputPlugin)
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
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Plugin (Hooks (..), Plugin (..))
import Skeletest.Prop.Internal (propPlugin)

runSkeletest :: [Plugin] -> [(FilePath, Spec)] -> IO ()
runSkeletest userPlugins testModules = handleUnknownErrors $ do
  Term.init
  selections <- loadCliArgs builtinFlags cliFlags
  resolveANSISupport

  setSnapshotRenderers snapshotRenderers

  let initialSpecs = map mkSpec testModules
  specs <- hooks.modifySpecRegistry selections pure initialSpecs
  when (null $ concatMap (getSpecTests . (.spec)) specs) $ do
    Term.outputErr $ Color.red "ERROR: No tests selected!"
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
  builtinFlags = foldMap (.cliFlags) builtinPlugins <> generalFlags
  generalFlags =
    [ flag @ANSIFlag
    , flag @(Maybe FormatFlag)
    ]

  mkSpec (specPath, spec) =
    SpecInfo
      { specPath
      , spec
      }

resolveANSISupport :: IO ()
resolveANSISupport = do
  CaptureOutputFlag captureOutput <- getFlag
  ANSIFlag mUseANSI <- getFlag
  traverse_ Term.setANSISupport $
    if
      | Just userANSI <- mUseANSI -> Just userANSI
      | not captureOutput -> Just False -- if --capture-output=off, ANSI could mess up output
      | otherwise -> Nothing
