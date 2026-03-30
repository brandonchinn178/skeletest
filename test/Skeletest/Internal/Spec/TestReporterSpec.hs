{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.Spec.TestReporterSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "TestResultMessageBox" $ do
    integration . it "renders correctly with ANSI" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ 1 `shouldBe` (2 :: Int)"
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith def{simulateANSI = True}
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "renders correctly with non-ANSI" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ 1 `shouldBe` (2 :: Int)"
        ]

      (stdout, stderr) <- expectFailure $ runner.runTestsWith def{simulateANSI = False}
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot
