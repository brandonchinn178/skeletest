module Skeletest.Internal.PluginTest (spec) where

import Skeletest

spec :: Spec
spec = do
  -- TODO: test plugin
  test "smoketest" $ do
    (1 + 2) `shouldBe` (3 :: Int)
