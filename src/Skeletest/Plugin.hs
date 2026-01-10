module Skeletest.Plugin (
  -- * Plugin
  Plugin (..),
  defaultPlugin,

  -- * Hooks
  Hooks (..),
  defaultHooks,

  -- * Re-exports

  -- ** TestResult
  TestResult (..),
  TestResultMessage (..),
  BoxSpec,
  BoxSpecContent (..),

  -- ** TestInfo
  TestInfo (..),

  -- ** Markers
  findMarker,
  hasMarkerNamed,
) where

import Control.Monad ((>=>))
import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Markers (findMarker, hasMarkerNamed)
import Skeletest.Internal.Snapshot (SnapshotRenderer)
import Skeletest.Internal.Spec.Output (BoxSpec, BoxSpecContent (..))
import Skeletest.Internal.Spec.Tree (SpecTree)
import Skeletest.Internal.TestInfo (TestInfo (..))
import Skeletest.Internal.TestRunner (TestResult (..), TestResultMessage (..))

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
  { hookModifyFileSpecs :: [SpecTree] -> IO [SpecTree]
  -- ^ Modify the specs in a file
  , hookRunTest :: TestInfo -> IO TestResult -> IO TestResult
  -- ^ Modify how a test is executed
  }

instance Semigroup Hooks where
  hooks1 <> hooks2 =
    Hooks
      { hookModifyFileSpecs = hookModifyFileSpecs hooks1 >=> hookModifyFileSpecs hooks2
      , hookRunTest = \testInfo -> hookRunTest hooks2 testInfo . hookRunTest hooks1 testInfo
      }

instance Monoid Hooks where
  mempty = defaultHooks

defaultHooks :: Hooks
defaultHooks =
  Hooks
    { hookModifyFileSpecs = pure
    , hookRunTest = \_ -> id
    }
