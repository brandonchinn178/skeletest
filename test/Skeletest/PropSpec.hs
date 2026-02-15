{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.PropSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range
import Skeletest.TestUtils.CallStack (sanitizeTraceback)
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "prop" $ do
    integration . it "shows hedgehog context for arbitrary failures" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Control.Exception"
        , "import Control.Monad.IO.Class (liftIO)"
        , "import Skeletest"
        , "import qualified Skeletest.Prop as Prop"
        , "import qualified Skeletest.Prop.Gen as Gen"
        , ""
        , "spec = do"
        , "  prop \"error\" $ do"
        , "    x <- forAll Gen.bool"
        , "    if x then liftIO $ throwIO MyException else pure ()"
        , ""
        , "data MyException = MyException deriving (Show)"
        , "instance Exception MyException where"
        , "  displayException MyException = \"this is MyException\""
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith zeroSeed
      stderr `shouldBe` ""
      sanitizeTraceback stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "renders Skeletest errors well" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = do"
        , "  prop \"error\" $ do"
        , "    _ <- getFlag @MyFlag"
        , "    pure ()"
        , ""
        , "data MyFlag = MyFlag"
        , "instance IsFlag MyFlag where"
        , "  flagName = \"my-flag\""
        , "  flagHelp = \"example\""
        , "  flagSpec = RequiredFlag (const $ Right MyFlag)"
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith zeroSeed
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "fails when configuration occurs after forAll" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Prop as Prop"
        , "import qualified Skeletest.Prop.Gen as Gen"
        , ""
        , "spec = prop \"discards\" $ do"
        , "  x <- forAll Gen.bool"
        , "  Prop.setDiscardLimit 10"
        , "  x `shouldBe` x"
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith zeroSeed
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "fails when configuration occurs after IO actions" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Prop as Prop"
        , "import qualified Skeletest.Prop.Gen as Gen"
        , ""
        , "spec = prop \"discards\" $ do"
        , "  FixtureTmpDir _ <- getFixture"
        , "  Prop.setDiscardLimit 10"
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith zeroSeed
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "setDiscardLimit" $ do
    integration . it "sets discard limit" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Prop as Prop"
        , ""
        , "spec = prop \"discards\" $ do"
        , "  Prop.setDiscardLimit 10"
        , "  discard"
        ]

      (stdout, stderr) <- expectFailure runner.runTests
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "===" $ do
    prop "checks two functions" $ do
      (read . show) P.=== id `shouldSatisfy` P.isoWith (Gen.int $ Range.exponential 0 10000000)
      (read . show) P.=== (+ 1) `shouldNotSatisfy` P.isoWith (Gen.int $ Range.exponential 0 10000000)

    integration . it "shows a helpful failure message" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
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

      (stdout, stderr) <- expectFailure $ runner.runTestsWith zeroSeed
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

zeroSeed :: TestArgs
zeroSeed = def{cliArgs = ["--seed=0:0"]}
