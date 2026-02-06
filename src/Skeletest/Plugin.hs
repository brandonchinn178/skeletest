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
  X.hasMarkerNamed,

  -- ** SpecRegistry
  X.SpecRegistry,
  X.Spec,
  X.SpecInfo (..),
  X.SpecTree,
  X.mapSpecTrees,
  X.traverseSpecTrees,
) where

import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Markers qualified as X
import Skeletest.Internal.Snapshot (SnapshotRenderer)
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
      { cliFlags = cliFlags plugin1 <> cliFlags plugin2
      , snapshotRenderers = snapshotRenderers plugin1 <> snapshotRenderers plugin2
      , hooks = hooks plugin1 <> hooks plugin2
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
  { hookModifySpecRegistry :: TestTargets -> (SpecRegistry -> IO SpecRegistry) -> (SpecRegistry -> IO SpecRegistry)
  -- ^ Modify all the specs in the test suite, being able to modify before/after
  -- previously registered hooks.
  --
  -- For example:
  -- @
  -- \_ modify -> pre >=> modify >=> post
  -- @
  , hookRunTest :: TestInfo -> IO TestResult -> IO TestResult
  -- ^ Modify how a test is executed
  }

instance Semigroup Hooks where
  hooks1 <> hooks2 =
    Hooks
      { hookModifySpecRegistry = \targets -> hookModifySpecRegistry hooks2 targets . hookModifySpecRegistry hooks1 targets
      , hookRunTest = \testInfo -> hookRunTest hooks2 testInfo . hookRunTest hooks1 testInfo
      }

instance Monoid Hooks where
  mempty = defaultHooks

defaultHooks :: Hooks
defaultHooks =
  Hooks
    { hookModifySpecRegistry = \_ -> id
    , hookRunTest = \_ -> id
    }
