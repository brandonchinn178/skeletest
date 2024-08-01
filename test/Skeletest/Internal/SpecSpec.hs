module Skeletest.Internal.SpecSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "skip" $ do
    integration . it "skips tests completely" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = skip \"broken tests\" $ do"
        , "  it \"should not run\" $ undefined"
        , "  it \"should not run either\" $ undefined"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner []
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "xfail" $ do
    integration . it "checks for expected failures" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = xfail \"broken tests\" $ do"
        , "  it \"should fail\" $ undefined"
        , "  it \"should fail too\" $ undefined"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner []
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "errors on unexpected passes" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = xfail \"broken tests\" $ do"
        , "  it \"should fail\" $ pure ()"
        , "  it \"should fail too\" $ pure ()"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "markManual" $ do
    integration . it "skips manual tests by default" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = do"
        , "  markManual . withMarkers [\"foo\"] $ do"
        , "    it \"foo1\" $ pure ()"
        , "    it \"foo2\" $ pure ()"
        , "  it \"bar1\" $ pure ()"
        , "  it \"bar2\" $ pure ()"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner []
      stderr `shouldBe` ""
      stdout
        `shouldSatisfy` P.and
          [ P.not $ P.hasInfix "foo1"
          , P.not $ P.hasInfix "foo2"
          , P.hasInfix "bar1"
          , P.hasInfix "bar2"
          ]

    integration . it "runs selected manual tests" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = do"
        , "  markManual . withMarkers [\"foo\"] $ do"
        , "    it \"foo1\" $ pure ()"
        , "    it \"foo2\" $ pure ()"
        , "  it \"bar1\" $ pure ()"
        , "  it \"bar2\" $ pure ()"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner ["*"]
      stderr `shouldBe` ""
      stdout
        `shouldSatisfy` P.and
          [ P.hasInfix "foo1"
          , P.hasInfix "foo2"
          , P.hasInfix "bar1"
          , P.hasInfix "bar2"
          ]

  describe "withMarkers" $ do
    integration . it "allows selecting from command line" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = do"
        , "  withMarkers [\"foo\"] $ do"
        , "    it \"foo1\" $ pure ()"
        , "    it \"foo2\" $ pure ()"
        , "  it \"bar1\" $ pure ()"
        , "  it \"bar2\" $ pure ()"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner ["@foo"]
      stderr `shouldBe` ""
      stdout
        `shouldSatisfy` P.and
          [ P.hasInfix "foo1"
          , P.hasInfix "foo2"
          , P.not $ P.hasInfix "bar1"
          , P.not $ P.hasInfix "bar2"
          ]

  describe "withMarker" $ do
    integration . it "allows selecting from command line" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "data MyMarker = MyMarker deriving (Show)"
        , "instance IsMarker MyMarker where getMarkerName _ = \"my-marker\""
        , ""
        , "spec = do"
        , "  withMarker MyMarker $ do"
        , "    it \"foo1\" $ pure ()"
        , "    it \"foo2\" $ pure ()"
        , "  it \"bar1\" $ pure ()"
        , "  it \"bar2\" $ pure ()"
        ]

      (stdout, stderr) <- expectSuccess $ runTests runner ["@my-marker"]
      stderr `shouldBe` ""
      stdout
        `shouldSatisfy` P.and
          [ P.hasInfix "foo1"
          , P.hasInfix "foo2"
          , P.not $ P.hasInfix "bar1"
          , P.not $ P.hasInfix "bar2"
          ]
