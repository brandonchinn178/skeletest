module Skeletest.Plugin (
  Plugin (..),
) where

import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Snapshot (SnapshotRenderer)

-- TODO: allow registering hooks to modify test execution
data Plugin = Plugin
  { cliFlags :: [Flag]
  , snapshotRenderers :: [SnapshotRenderer]
  }

instance Semigroup Plugin where
  plugin1 <> plugin2 =
    Plugin
      { cliFlags = cliFlags plugin1 <> cliFlags plugin2
      , snapshotRenderers = snapshotRenderers plugin1 <> snapshotRenderers plugin2
      }

instance Monoid Plugin where
  mempty =
    Plugin
      { cliFlags = []
      , snapshotRenderers = []
      }
