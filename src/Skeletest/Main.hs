{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Main (
  runSkeletest,
  SkeletestOptions (..),
  defaultOptions,

  -- * CLI flags
  Flag,
  flag,

  -- * Snapshots
  SnapshotRenderer (..),

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
import Skeletest.Internal.Snapshot (SnapshotRenderer (..), SnapshotUpdateFlag)
import Skeletest.Internal.Spec (
  Spec,
  SpecInfo (..),
  applyTestSelections,
  pruneSpec,
  runSpecs,
 )

-- TODO: a plugin should return a SkeletestOptions to merge with the other options
type Plugin = ()

data SkeletestOptions = SkeletestOptions
  { cliFlags :: [Flag]
  , snapshotRenderers :: [SnapshotRenderer]
  , plugins :: [Plugin]
  }

-- TODO: allow modifying command line options, test execution, etc.
defaultOptions :: SkeletestOptions
defaultOptions =
  SkeletestOptions
    { cliFlags = []
    , snapshotRenderers = []
    , plugins = []
    }

-- TODO: handle plugins
runSkeletest :: SkeletestOptions -> [(FilePath, String, Spec)] -> IO ()
runSkeletest SkeletestOptions{..} testModules = do
  selections <- loadCliArgs builtinFlags cliFlags
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
