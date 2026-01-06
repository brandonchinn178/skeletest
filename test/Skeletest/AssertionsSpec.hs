{-# LANGUAGE CPP #-}

module Skeletest.AssertionsSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "shouldBe" $ do
    it "should pass" $
      1 `shouldBe` (1 :: Int)

    integration . it "should show helpful failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ 1 `shouldBe` (2 :: Int)"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "shouldNotBe" $ do
    it "should pass" $
      1 `shouldNotBe` (2 :: Int)

    integration . it "should show helpful failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ 1 `shouldNotBe` (1 :: Int)"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "shouldSatisfy" $ do
    it "should pass" $
      1 `shouldSatisfy` P.gt (0 :: Int)

    integration . it "should show helpful failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Predicate as P"
        , ""
        , "spec = it \"should fail\" $ (-1) `shouldSatisfy` P.gt (0 :: Int)"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "shouldNotSatisfy" $ do
    it "should pass" $
      (-1) `shouldNotSatisfy` P.gt (0 :: Int)

    integration . it "should show helpful failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import qualified Skeletest.Predicate as P"
        , ""
        , "spec = it \"should fail\" $ 1 `shouldNotSatisfy` P.gt (0 :: Int)"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "context" $ do
    integration . it "should show failure context" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ do"
        , "  context \"hello\" . context \"world\" $"
        , "    1 `shouldBe` (2 :: Int)"
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "failTest" $ do
    integration . it "should show failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "spec = it \"should fail\" $ failTest \"error message\""
        ]

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

  integration . it "shows backtrace of failed assertions" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should fail\" $ expectPositive (-1)"
      , ""
      , "expectPositive :: HasCallStack => Int -> IO ()"
      , "expectPositive = expectGT 0"
      , ""
      , "expectGT :: HasCallStack => Int -> Int -> IO ()"
      , "expectGT x actual = actual `shouldSatisfy` P.gt x"
      ]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

  integration . it "shows helpful error on pattern match fail" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should fail\" $ do"
      , "  Just x <- pure Nothing"
      , "  x `shouldBe` True"
      ]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    stdout `shouldSatisfy` P.matchesSnapshot

  integration . it "shows unrecognized exceptions" $ do
    runner <- getFixture
    addTestFile runner "ExampleSpec.hs" $
      [ "module ExampleSpec (spec) where"
      , ""
      , "import Skeletest"
      , "import qualified Skeletest.Predicate as P"
      , ""
      , "spec = it \"should fail\" $ do"
      , "  _ <- readFile \"unknown-file.txt\""
      , "  pure ()"
      ]

    (code, stdout, stderr) <- runTests runner []
    code `shouldBe` ExitFailure 1
    stderr `shouldBe` ""
    sanitizeTraceback stdout `shouldSatisfy` P.matchesSnapshot

-- GHC 9.10 specifically added a backtrace to SomeException, which was reverted in 9.12
-- https://github.com/haskell/core-libraries-committee/issues/285
sanitizeTraceback :: String -> String
#if __GLASGOW_HASKELL__ == 910
sanitizeTraceback s =
  let (pre, post) = span (/= "HasCallStack backtrace:") $ lines s
      (_, post2) = break isEnd post
   in unlines $ pre ++ post2
  where
    isEnd ('-' : _) = True
    isEnd _ = False
#else
sanitizeTraceback = id
#endif
