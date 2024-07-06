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
import Skeletest.Internal.Spec (Spec, SpecTree (..), filterSpec, runSpecs)

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
  _ <- loadCliArgs (builtinFlags <> cliFlags)
  let initialSpecs = map mkSpec testModules
  success <- runSpecs . pruneSpec . selectTests $ initialSpecs
  unless success exitFailure
  where
    builtinFlags =
      [ flag @SnapshotUpdateFlag
      ]

    mkSpec (fp, name, spec) =
      let name' = stripSuffix "Spec" $ Text.pack name
       in (fp, name', spec)

    -- TODO: allow filtering test tree by filepath, name of test, etc. in CLI arguments
    selectTests = id

    -- remove empty specs
    pruneSpec specs =
      [ (fp, name, filterSpec (not . isEmptySpec) spec)
      | (fp, name, spec) <- specs
      , let
          isEmptySpec = \case
            SpecGroup _ [] -> True
            _ -> False
      ]

    -- same as Text.stripSuffix, except return original string if not match
    stripSuffix suf s = fromMaybe s $ Text.stripSuffix suf s
