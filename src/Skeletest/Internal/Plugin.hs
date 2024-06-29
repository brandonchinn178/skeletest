module Skeletest.Internal.Plugin (
  plugin,
) where

import GHC.Plugins

-- | The plugin to convert a test file. Injected by the preprocessor.
plugin :: Plugin
plugin = defaultPlugin
