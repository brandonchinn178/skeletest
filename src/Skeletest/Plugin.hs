module Skeletest.Plugin (
  Plugin (..),
  defaultPlugin,

  -- * Hooks
  Hooks (..),
  defaultHooks,

  -- * Re-exports

  -- ** TestResult
  TestResult (..),
  TestResultMessage (..),

  -- ** Markers
  findMarker,
) where

import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Markers (findMarker)
import Skeletest.Internal.Snapshot (SnapshotRenderer)
import Skeletest.Internal.TestInfo (TestInfo)
import Skeletest.Internal.TestRunner (TestResult (..), TestResultMessage (..))

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

data Hooks = Hooks
  { hookRunTest :: TestInfo -> IO TestResult -> IO TestResult
  }

instance Semigroup Hooks where
  hooks1 <> hooks2 =
    Hooks
      { hookRunTest = \testInfo -> hookRunTest hooks2 testInfo . hookRunTest hooks1 testInfo
      }

instance Monoid Hooks where
  mempty = defaultHooks

defaultHooks :: Hooks
defaultHooks =
  Hooks
    { hookRunTest = \_ -> id
    }
