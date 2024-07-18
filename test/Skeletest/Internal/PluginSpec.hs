module Skeletest.Internal.PluginSpec (spec) where

import Skeletest

spec :: Spec
spec = do
  -- FIXME: test plugin
  test "smoketest" $ do
    (1 + 2) `shouldBe` (3 :: Int)
