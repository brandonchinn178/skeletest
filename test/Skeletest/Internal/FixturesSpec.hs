{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.FixturesSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "getFixture" $ do
    integration . it "detects circular dependencies" $ do
      runner <- getFixture @TestRunner

      -- Fixture graph:
      --   A
      --   -> B
      --      -> C
      --      -> D
      --         -> A
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "data FixtureA = FixtureA"
        , "data FixtureB = FixtureB"
        , "data FixtureC = FixtureC"
        , "data FixtureD = FixtureD"
        , ""
        , "instance Fixture FixtureA where"
        , "  fixtureAction = do"
        , "    FixtureB <- getFixture"
        , "    pure . noCleanup $ FixtureA"
        , ""
        , "instance Fixture FixtureB where"
        , "  fixtureAction = do"
        , "    FixtureC <- getFixture"
        , "    FixtureD <- getFixture"
        , "    pure . noCleanup $ FixtureB"
        , ""
        , "instance Fixture FixtureC where"
        , "  fixtureAction = do"
        , "    pure . noCleanup $ FixtureC"
        , ""
        , "instance Fixture FixtureD where"
        , "  fixtureAction = do"
        , "    FixtureA <- getFixture"
        , "    pure . noCleanup $ FixtureD"
        , ""
        , "spec = it \"should error\" $ do"
        , "  FixtureA <- getFixture"
        , "  pure ()"
        ]

      (stdout, stderr) <- expectFailure runner.runTests
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    -- https://github.com/brandonchinn178/skeletest/issues/72
    integration . it "throws the appropriate error when setup fails" $ do
      runner <- getFixture @TestRunner

      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "data FixtureA = FixtureA"
        , ""
        , "instance Fixture FixtureA where"
        , "  fixtureScope = PerSessionFixture"
        , "  fixtureAction = failTest \"Fixture setup failed\""
        , ""
        , "spec = do"
        , "  it \"should error\" $ do"
        , "    FixtureA <- getFixture"
        , "    pure ()"
        , "  it \"should error again\" $ do"
        , "    FixtureA <- getFixture"
        , "    pure ()"
        ]

      (stdout, stderr) <- expectFailure runner.runTests
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot
