module Skeletest.PropSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "setDiscardLimit" $ do
    integration . it "sets discard limit" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = prop \"discards\" $ do"
        , "  setDiscardLimit 10"
        , "  discard"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "===" $ do
    prop "checks two functions" $ do
      (read . show) P.=== id `shouldSatisfy` P.isoWith (Gen.int $ Range.exponential 0 10000000)
      (read . show) P.=== (+ 1) `shouldNotSatisfy` P.isoWith (Gen.int $ Range.exponential 0 10000000)

    integration . it "shows a helpful failure message" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Predicate as P"
        , "import qualified Skeletest.Prop.Gen as Gen"
        , "import qualified Skeletest.Prop.Range as Range"
        , ""
        , "spec = do"
        , "  prop \"is isomorphic\" $ do"
        , "    (read . show) P.=== (+ 1) `shouldSatisfy` P.isoWith (Gen.int $ Range.linear 0 10)"
        , "  prop \"is not isomorphic\" $ do"
        , "    (read . show) P.=== id `shouldNotSatisfy` P.isoWith (Gen.int $ Range.linear 0 10)"
        ]

      (code, stdout, stderr) <- runTests runner ["--seed=0:0"]
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot
