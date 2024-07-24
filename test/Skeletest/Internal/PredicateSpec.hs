module Skeletest.Internal.PredicateSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Skeletest
import Skeletest.Predicate (PredicateResult (..), runPredicate)
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

one :: Int
one = 1

data User = User
  { name :: String
  , age :: Maybe Int
  }

spec :: Spec
spec = do
  describe "General" $ do
    describe "anything" $ do
      it "matches anything" $ do
        one `shouldSatisfy` P.anything
        "hello" `shouldSatisfy` P.anything

  describe "Ord" $ do
    describe "eq" $ do
      it "checks equality" $ do
        one `shouldSatisfy` P.eq 1
        one `shouldNotSatisfy` P.eq 2

    describe "gt" $ do
      it "checks inequality" $ do
        one `shouldSatisfy` P.gt 0
        one `shouldNotSatisfy` P.gt 2

  describe "Data types" $ do
    describe "just" $ do
      it "checks Maybe" $ do
        Just one `shouldSatisfy` P.just (P.gt 0)
        Just one `shouldNotSatisfy` P.just (P.gt 2)
        Nothing `shouldNotSatisfy` P.just P.anything

    describe "left" $ do
      it "checks Either" $ do
        Left one `shouldSatisfy` P.left (P.gt 0)
        Left one `shouldNotSatisfy` P.left (P.gt 2)
        Right one `shouldNotSatisfy` P.left P.anything

    describe "con" $ do
      it "checks record fields" $ do
        User "alice" (Just 10) `shouldSatisfy` P.con User{name = P.eq "alice", age = P.just (P.gt 0)}
        User "alice" (Just 10) `shouldNotSatisfy` P.con User{name = P.eq "", age = P.just (P.gt 0)}

      it "accepts anything in omitted record fields" $ do
        User "alice" (Just 10) `shouldSatisfy` P.con User{age = P.just (P.gt 0)}

      it "checks positional fields" $ do
        User "alice" (Just 10) `shouldSatisfy` P.con (User (P.eq "alice") (P.just (P.gt 0)))
        User "alice" (Just 10) `shouldNotSatisfy` P.con (User (P.eq "") (P.just (P.gt 0)))

        -- works with dollar sign
        User "alice" (Just 10) `shouldSatisfy` (P.con $ User (P.eq "alice") (P.just (P.gt 0)))

      integration . it "shows a helpful failure message" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "data User = User { name :: String }"
          , ""
          , "spec = it \"should error\" $ do"
          , "  User \"alice\" `shouldSatisfy` P.con User{name = P.eq \"\"}"
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stderr `shouldBe` ""
        stdout `shouldSatisfy` P.matchesSnapshot

      integration . it "fails to compile with unknown record field" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "data User = User { name :: String }"
          , ""
          , "spec = it \"should error\" $ do"
          , "  User \"alice\" `shouldSatisfy` P.con User{foo = P.eq \"\"}"
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stdout `shouldBe` ""
        stderr `shouldSatisfy` P.matchesSnapshot

      integration . it "fails to compile with omitted positional fields" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "data User = User { name :: String, age :: Maybe Int }"
          , ""
          , "spec = it \"should error\" $ do"
          , "  User \"alice\" (Just 1) `shouldSatisfy` P.con (User (P.eq \"\"))"
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stdout `shouldBe` ""
        stderr `shouldSatisfy` P.matchesSnapshot

      integration . it "fails to compile with non-constructor" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "spec = it \"should error\" $ do"
          , "  \"\" `shouldSatisfy` P.con \"\""
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stdout `shouldBe` ""
        stderr `shouldSatisfy` P.matchesSnapshot

      integration . it "fails to compile when not applied to anything" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "spec = it \"should error\" $ do"
          , "  \"\" `shouldSatisfy` P.con"
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stdout `shouldBe` ""
        stderr `shouldSatisfy` P.matchesSnapshot

      integration . it "fails to compile when applied to multiple arguments" $ do
        runner <- getFixture
        addTestFile runner "ExampleSpec.hs" $
          [ "module ExampleSpec (spec) where"
          , ""
          , "import Skeletest"
          , "import qualified Skeletest.Predicate as P"
          , ""
          , "spec = it \"should error\" $ do"
          , "  \"\" `shouldSatisfy` P.con 1 2"
          ]

        (code, stdout, stderr) <- runTests runner []
        code `shouldBe` ExitFailure 1
        stdout `shouldBe` ""
        stderr `shouldSatisfy` P.matchesSnapshot

  describe "Numeric" $ do
    describe "approx" $ do
      let x = 0.1 + 0.2 :: Double

      it "checks approximate equality" $ do
        x `shouldSatisfy` P.approx P.tol 0.3
        x `shouldNotSatisfy` P.approx P.tol 0.5

      it "allows setting tolerance" $ do
        -- with relative
        x `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6} 0.3
        x `shouldSatisfy` P.approx P.tol{P.abs = 1e-12} 0.3
        x `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6, P.abs = 1e-12} 0.3

        -- without relative
        x `shouldSatisfy` P.approx P.tol{P.rel = Nothing} 0.3
        x `shouldSatisfy` P.approx P.tol{P.rel = Nothing, P.abs = 1e-12} 0.3

  describe "Combinators" $ do
    describe "<<<" $ do
      it "transforms the input" $ do
        one `shouldSatisfy` (P.gt 5 P.<<< (* 10))

      it "shows a helpful failure message" $ do
        runPredicate (P.gt 10 P.<<< (* 2)) one `shouldSatisfy` P.returns (P.con $ PredicateFail P.matchesSnapshot)

    describe ">>>" $ do
      it "transforms the input" $ do
        one `shouldSatisfy` (show P.>>> P.eq "1")

      it "shows a helpful failure message" $ do
        runPredicate (show P.>>> P.eq "2") one `shouldSatisfy` P.returns (P.con $ PredicateFail P.matchesSnapshot)

    describe "not" $ do
      it "negates a predicate" $ do
        one `shouldSatisfy` P.not (P.gt 10)
        one `shouldNotSatisfy` P.not (P.gt 0)

    describe "and" $ do
      it "checks all predicates are true" $ do
        one `shouldSatisfy` P.and [P.eq 1, P.gt 0]
        one `shouldNotSatisfy` P.and [P.eq 1, P.gt 10]
        one `shouldNotSatisfy` P.and [P.eq 2, P.gt 0]
        one `shouldNotSatisfy` P.and [P.eq 2, P.gt 10]

  describe "Subsequences" $ do
    describe "hasPrefix" $ do
      it "checks prefix" $ do
        "hello world" `shouldSatisfy` P.hasPrefix "hello"
        "hello world" `shouldNotSatisfy` P.hasPrefix "world"

    describe "hasInfix" $ do
      it "checks infix" $ do
        ">> hello world <<" `shouldSatisfy` P.hasInfix "hello"
        ">> hello world <<" `shouldNotSatisfy` P.hasInfix "!!"

    describe "hasSuffix" $ do
      it "checks suffix" $ do
        "hello world" `shouldSatisfy` P.hasSuffix "world"
        "hello world" `shouldNotSatisfy` P.hasSuffix "hello"

  describe "IO" $ do
    describe "returns" $ do
      it "checks result" $ do
        let action = do
              ref <- newIORef Nothing
              writeIORef ref (Just one)
              readIORef ref
        action `shouldSatisfy` P.returns (P.just (P.gt 0))
        action `shouldNotSatisfy` P.returns (P.just (P.gt 10))
