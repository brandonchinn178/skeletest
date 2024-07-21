module Skeletest.Internal.SnapshotSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  integration . it "detects corrupted snapshot files" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should error\" $ do"
      , "  \"\" `shouldSatisfy` P.matchesSnapshot"
      ]
    addTestFile runner "__snapshots__/ExampleSpec.snap.md" ["asdf"]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot
