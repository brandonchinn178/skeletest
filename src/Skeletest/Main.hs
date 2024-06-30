module Skeletest.Main (
  runSkeletest,
  SkeletestOptions (..),
  defaultOptions,

  -- * Plugins
  Plugin,

  -- * Re-exports
  Spec,
) where

import Skeletest.Internal.Spec (Spec)

-- TODO: enable users to define plugins that modify command line options,
-- test execution, etc.
type Plugin = ()

data SkeletestOptions = SkeletestOptions
  { plugins :: [Plugin]
  }

defaultOptions :: SkeletestOptions
defaultOptions =
  SkeletestOptions
    { plugins = []
    }

runSkeletest :: SkeletestOptions -> [Spec] -> IO ()
runSkeletest _ _ = putStrLn "TODO: runSkeletest"
