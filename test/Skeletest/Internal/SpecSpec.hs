module Skeletest.Internal.SpecSpec (spec) where

import Skeletest

spec :: Spec
spec = do
  -- FIXME: change these to integration tests
  describe "Markers" $ do
    withMarkers ["foo", "bar"] $ do
      it "foo" $ pure ()
      it "bar" $ pure ()

    withManualMarkers ["manual"] $ do
      it "manual1" $ pure ()
      it "manual2" $ pure ()

    xfail "bad test" $ it "xfail" $ 1 `shouldBe` (2 :: Int)

    -- xfail "bad test" $ it "xpass" $ pure ()

    skip "bad test" $ it "skip" $ pure ()
