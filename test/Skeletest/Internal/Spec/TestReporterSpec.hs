{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.Spec.TestReporterSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  integration . describe "Report format" $ do
    sequence_
      [ it ("renders --format=" <> format <> " with " <> ansiLabel) $ do
          runner <- getFixture @TestRunner
          runner.addTestFile "ExampleSpec.hs" $
            [ "module ExampleSpec (spec) where"
            , ""
            , "import Skeletest"
            , ""
            , "spec = do"
            , "  it \"should pass\" $ 1 `shouldBe` (1 :: Int)"
            , "  it \"should fail\" $ 1 `shouldBe` (2 :: Int)"
            , "  skip \"no run\" . it \"should skip\" $ pure ()"
            ]
          (_, stdout, stderr) <-
            runner.runTestsWith
              def
                { simulateANSI = useANSI
                , cliArgs = ["--format=" <> format]
                }
          stderr `shouldBe` ""
          stdout `shouldSatisfy` P.matchesSnapshot
      | useANSI <- [True, False]
      , let ansiLabel = if useANSI then "ANSI" else "non-ANSI"
      , format <- ["minimal", "full", "verbose"]
      ]
