{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The module that contains everything needed to implement a plugin.
module Skeletest.Plugin (
  -- * Plugin
  Plugin (..),
  defaultPlugin,

  -- * Hooks
  module Skeletest.Hooks,
) where

import Skeletest.Hooks
import Skeletest.Internal.CLI (Flag)
import Skeletest.Internal.Snapshot.Renderer (SnapshotRenderer)

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
