module Skeletest.MainSpec (spec) where

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
    stderr `shouldSatisfy` P.matchesSnapshot

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
    stderr `shouldSatisfy` P.matchesSnapshot

minimalTest :: String -> FileContents
minimalTest name =
  [ "module " <> name <> " (spec) where"
  , "import Skeletest"
  , "spec = it \"should run\" $ pure ()"
  ]
