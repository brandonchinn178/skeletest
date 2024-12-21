{-# LANGUAGE CPP #-}

module Skeletest.MainSpec (spec) where

import Data.Text qualified as Text
import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  integration . it "errors if Skeletest.Main not imported" $ do
    runner <- getFixture
    setMainFile runner []
    addTestFile runner "ExampleSpec.hs" (minimalTest "ExampleSpec")

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stdout `shouldBe` ""
    normalizePluginError stderr `shouldSatisfy` P.matchesSnapshot

  integration . it "ignores non-test files" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , "import Skeletest"
      , "import TestUtils"
      , "spec = it \"should run\" $ testUserName `shouldBe` \"Alice\""
      ]
    addTestFile runner "TestUtils.hs" $
      [ "module TestUtils where"
      , "testUserName = \"Alice\""
      ]

    _ <- expectSuccess $ runTests runner []
    pure ()

  integration . it "errors if main function defined" $ do
    runner <- getFixture
    setMainFile runner $
      [ "import Skeletest.Main"
      , ""
      , "main = putStrLn \"hello world\""
      ]
    addTestFile runner "ExampleSpec.hs" (minimalTest "ExampleSpec")

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stdout `shouldBe` ""
    normalizeGhc29916 stderr `shouldSatisfy` P.matchesSnapshot

minimalTest :: String -> FileContents
minimalTest name =
  [ "module " <> name <> " (spec) where"
  , "import Skeletest"
  , "spec = it \"should run\" $ pure ()"
  ]

normalizePluginError :: String -> String
normalizePluginError = Text.unpack . go . Text.pack
  where
    replace old new = Text.replace (Text.pack old) (Text.pack new)
    go
      | __GLASGOW_HASKELL__ == (908 :: Int) = replace "*** Exception: ExitFailure 1" "\n*** Exception: ExitFailure 1"
      | otherwise = id

normalizeGhc29916 :: String -> String
normalizeGhc29916 = Text.unpack . go . Text.pack
  where
    replace old new = Text.replace (Text.pack old) (Text.pack new)
    go
      | __GLASGOW_HASKELL__ == (908 :: Int) = replace "<generated>" "<no location info>"
      | otherwise = id
