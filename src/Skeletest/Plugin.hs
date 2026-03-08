{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Plugin (
  -- * Plugin
  Plugin (..),
  defaultPlugin,

  -- * Hooks
  Hooks (..),
  defaultHooks,

  -- * Re-exports

  -- ** TestResult
  X.TestResult (..),
  X.TestResultMessage (..),
  X.BoxSpec,
  X.BoxSpecContent (..),

  -- ** TestInfo
  X.TestInfo (..),

  -- ** Markers
  X.findMarker,
  X.hasMarker,
  X.hasMarkerNamed,

  -- ** SpecRegistry
  X.SpecRegistry,
  X.Spec,
  X.SpecInfo (..),
  X.SpecTree (..),
  X.SpecTest (..),
  X.getSpecTrees,
  X.withSpecTrees,
  X.mapSpecTrees,
  X.traverseSpecTrees,
  X.mapSpecTests,
  X.traverseSpecTests,
  X.filterSpecTests,
  X.mapSpecs,
  X.traverseSpecs,
) where

import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Markers qualified as X
import Skeletest.Internal.Snapshot.Renderer (SnapshotRenderer)
import Skeletest.Internal.Spec.Output qualified as X
import Skeletest.Internal.Spec.Tree (SpecRegistry)
import Skeletest.Internal.Spec.Tree qualified as X
import Skeletest.Internal.TestInfo (TestInfo (..))
import Skeletest.Internal.TestInfo qualified as X
import Skeletest.Internal.TestRunner (TestResult (..))
import Skeletest.Internal.TestRunner qualified as X
import Skeletest.Internal.TestTargets (TestTargets)

-- | A plugin for extending Skeletest.
--
-- Use 'defaultPlugin' instead of using v'Plugin' directly, to minimize
-- breaking changes.
data Plugin = Plugin
  { cliFlags :: [Flag]
  , snapshotRenderers :: [SnapshotRenderer]
  , hooks :: Hooks
  }

instance Semigroup Plugin where
  plugin1 <> plugin2 =
    Plugin
      { cliFlags = plugin1.cliFlags <> plugin2.cliFlags
      , snapshotRenderers = plugin1.snapshotRenderers <> plugin2.snapshotRenderers
      , hooks = plugin1.hooks <> plugin2.hooks
      }

instance Monoid Plugin where
  mempty = defaultPlugin

defaultPlugin :: Plugin
defaultPlugin =
  Plugin
    { cliFlags = []
    , snapshotRenderers = []
    , hooks = defaultHooks
    }

-- | Hooks for extending Skeletest.
--
-- Use 'defaultHooks' instead of using v'Hooks' directly, to minimize
-- breaking changes.
data Hooks = Hooks
  { modifySpecRegistry :: TestTargets -> (SpecRegistry -> IO SpecRegistry) -> (SpecRegistry -> IO SpecRegistry)
  -- ^ Modify all the specs in the test suite, being able to modify before/after
  -- previously registered hooks.
  --
  -- For example:
  -- @
  -- \_ modify -> pre >=> modify >=> post
  -- @
  --
  -- @since 0.3.4
  , runTest :: TestInfo -> IO TestResult -> IO TestResult
  -- ^ Modify how a test is executed
  }

instance Semigroup Hooks where
  hooks1 <> hooks2 =
    Hooks
      { modifySpecRegistry = \targets -> hooks2.modifySpecRegistry targets . hooks1.modifySpecRegistry targets
      , runTest = \testInfo -> hooks2.runTest testInfo . hooks1.runTest testInfo
      }

instance Monoid Hooks where
  mempty = defaultHooks

defaultHooks :: Hooks
defaultHooks =
  Hooks
    { modifySpecRegistry = \_ -> id
    , runTest = \_ -> id
    }
