{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Main (
  runSkeletest,
  SkeletestOptions (..),
  defaultOptions,

  -- * Plugins
  Plugin,

  -- * Re-exports
  Spec,
) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as Text

import Skeletest.Internal.Spec (Spec, SpecTree (..), filterSpec, runSpec)
import Skeletest.Internal.Spec qualified as Spec

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

-- TODO: handle plugins
runSkeletest :: SkeletestOptions -> [(String, Spec)] -> IO ()
runSkeletest _ specs = do
  let fullSpec = mapM_ mkSpec specs
  let specToRun = pruneSpec . selectTests $ fullSpec
  runSpec specToRun
  where
    mkSpec (name, spec) =
      let name' = fromMaybe name $ (fmap Text.unpack . Text.stripSuffix "Test" . Text.pack) name
       in Spec.describe name' spec

    -- TODO: allow filtering test tree by filepath, name of test, etc. in CLI arguments
    selectTests = id

    -- remove empty specs
    pruneSpec = filterSpec $ \case
      SpecGroup _ [] -> False
      _ -> True
