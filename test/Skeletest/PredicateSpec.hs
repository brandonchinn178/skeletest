{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Skeletest.PredicateSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import Skeletest
import Skeletest.Predicate (PredicateResult (..), runPredicate)
import Skeletest.Predicate qualified as P
import UnliftIO.Exception (Exception, throwIO)

import Skeletest.TestUtils.Integration

data User = User
  { name :: String
  , age :: Maybe Int
  }

data HttpException = HttpException Int
  deriving (Show)

instance Exception HttpException

spec :: Spec
spec = do
  describe "General" $ do
    describe "anything" $ do
      it "matches anything" $ do
        1 `shouldSatisfy` P.anything
        "hello" `shouldSatisfy` P.anything

  describe "Ord" $ do
    describe "eq" $ do
      it "checks equality" $ do
        1 `shouldSatisfy` P.eq 1
        1 `shouldNotSatisfy` P.eq 2

      it "shows helpful failure messages" $ do
        snapshotFailure (P.eq 1) 2
        snapshotFailure (P.not $ P.eq 1) 1

    describe "gt" $ do
      it "checks inequality" $ do
        1 `shouldSatisfy` P.gt 0
        1 `shouldNotSatisfy` P.gt 1
        1 `shouldNotSatisfy` P.gt 2

    describe "gte" $ do
      it "checks inequality" $ do
        1 `shouldSatisfy` P.gte 0
        1 `shouldSatisfy` P.gte 1
        1 `shouldNotSatisfy` P.gte 2

    describe "lt" $ do
      it "checks inequality" $ do
        1 `shouldSatisfy` P.lt 2
        1 `shouldNotSatisfy` P.lt 1
        1 `shouldNotSatisfy` P.lt 0

    describe "lte" $ do
      it "checks inequality" $ do
        1 `shouldSatisfy` P.lte 2
        1 `shouldSatisfy` P.lte 1
        1 `shouldNotSatisfy` P.lte 0

  describe "Data types" $ do
    describe "just" $ do
      it "checks Maybe" $ do
        Just 1 `shouldSatisfy` P.just (P.gt 0)
        Just 1 `shouldNotSatisfy` P.just (P.gt 2)
        Nothing `shouldNotSatisfy` P.just P.anything

    describe "nothing" $ do
      it "checks Maybe" $ do
        Nothing `shouldSatisfy` P.nothing
        Just 1 `shouldNotSatisfy` P.nothing

    describe "left" $ do
      it "checks Either" $ do
        Left 1 `shouldSatisfy` P.left (P.gt 0)
        Left 1 `shouldNotSatisfy` P.left (P.gt 2)
        Right 1 `shouldNotSatisfy` P.left P.anything

    describe "right" $ do
      it "checks Either" $ do
        Right 1 `shouldSatisfy` P.right (P.gt 0)
        Right 1 `shouldNotSatisfy` P.right (P.gt 2)
        Left 1 `shouldNotSatisfy` P.right P.anything

    describe "tup" $ do
      it "checks all predicates" $ do
        (1, "hello") `shouldSatisfy` P.tup (P.eq 1, P.hasPrefix "he")
        (1, "hello") `shouldNotSatisfy` P.tup (P.eq 1, P.hasPrefix "xx")
        (1, "hello") `shouldNotSatisfy` P.tup (P.eq 0, P.hasPrefix "he")
        (1, "hello") `shouldNotSatisfy` P.tup (P.eq 0, P.hasPrefix "xx")

        -- some longer tuples
        (1, True, "hello") `shouldSatisfy` P.tup (P.eq 1, P.eq True, P.eq "hello")
        (1, True, "hello", 1.2) `shouldSatisfy` P.tup (P.eq 1, P.eq True, P.eq "hello", P.gt 0)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.tup (P.eq 0, P.eq "")) (1, "")
        snapshotFailure (P.not $ P.tup (P.eq 1, P.eq "")) (1, "")

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
        normalizeConFailure stderr `shouldSatisfy` P.matchesSnapshot

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
        1 `shouldSatisfy` (P.gt 5 P.<<< (* 10))

      it "shows a helpful failure message" $ do
        snapshotFailure (P.gt 10 P.<<< (* 2)) 1

    describe ">>>" $ do
      it "transforms the input" $ do
        1 `shouldSatisfy` (show P.>>> P.eq "1")

      it "shows a helpful failure message" $ do
        snapshotFailure (show P.>>> P.eq "2") 1

    describe "not" $ do
      it "negates a predicate" $ do
        1 `shouldSatisfy` P.not (P.gt 10)
        1 `shouldNotSatisfy` P.not (P.gt 0)

    describe "&&" $ do
      it "checks both predicates are true" $ do
        1 `shouldSatisfy` (P.eq 1 P.&& P.gt 0)
        1 `shouldNotSatisfy` (P.eq 1 P.&& P.gt 10)
        1 `shouldNotSatisfy` (P.eq 2 P.&& P.gt 0)
        1 `shouldNotSatisfy` (P.eq 2 P.&& P.gt 10)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.eq 2 P.&& P.gt 0) 1
        snapshotFailure (P.not $ P.eq 1 P.&& P.gt 0) 1

    describe "||" $ do
      it "checks either predicate is true" $ do
        1 `shouldSatisfy` (P.eq 1 P.|| P.gt 0)
        1 `shouldSatisfy` (P.eq 1 P.|| P.gt 10)
        1 `shouldSatisfy` (P.eq 2 P.|| P.gt 0)
        1 `shouldNotSatisfy` (P.eq 2 P.|| P.gt 10)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.eq 2 P.|| P.gt 1) 1
        snapshotFailure (P.not $ P.eq 2 P.|| P.gt 0) 1

    describe "and" $ do
      it "checks all predicates are true" $ do
        1 `shouldSatisfy` P.and [P.eq 1, P.gt 0]
        1 `shouldNotSatisfy` P.and [P.eq 1, P.gt 10]
        1 `shouldNotSatisfy` P.and [P.eq 2, P.gt 0]
        1 `shouldNotSatisfy` P.and [P.eq 2, P.gt 10]

      it "shows helpful failure messages" $ do
        snapshotFailure (P.and [P.eq 2, P.gt 0, P.lt 10]) 1
        snapshotFailure (P.not $ P.and [P.eq 1, P.gt 0, P.lt 10]) 1

    describe "or" $ do
      it "checks any predicate is true" $ do
        1 `shouldSatisfy` P.or [P.eq 1, P.gt 0]
        1 `shouldSatisfy` P.or [P.eq 1, P.gt 10]
        1 `shouldSatisfy` P.or [P.eq 2, P.gt 0]
        1 `shouldNotSatisfy` P.or [P.eq 2, P.gt 10]

      it "shows helpful failure messages" $ do
        snapshotFailure (P.or [P.eq 2, P.gt 1, P.lt 0]) 1
        snapshotFailure (P.not $ P.or [P.eq 2, P.gt 0, P.lt 0]) 1

  describe "Containers" $ do
    describe "any" $ do
      it "checks predicate is true for any value" $ do
        [1, 2, 3] `shouldSatisfy` P.any (P.eq 2)
        [1, 2, 3] `shouldNotSatisfy` P.any (P.eq 10)
        [] `shouldNotSatisfy` P.any (P.eq 10)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.any (P.eq 2)) []
        snapshotFailure (P.not $ P.any (P.eq 2)) [1, 2, 3]

    describe "all" $ do
      it "checks predicate is true for all values" $ do
        [] `shouldSatisfy` P.all (P.gt 0)
        [1, 2, 3] `shouldSatisfy` P.all (P.gt 0)
        [1, 2, 3] `shouldNotSatisfy` P.all (P.lt 3)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.all (P.gt 10)) [1, 2]
        snapshotFailure (P.not $ P.all (P.gt 0)) [1, 2, 3]

    describe "elem" $ do
      it "checks element is in the given container" $ do
        [1, 2, 3] `shouldSatisfy` P.elem 1
        [1, 2, 3] `shouldNotSatisfy` P.elem 10

      it "shows helpful failure messages" $ do
        snapshotFailure (P.elem 1) []
        snapshotFailure (P.not $ P.elem 1) [1]

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
              writeIORef ref (Just 1)
              readIORef ref
        action `shouldSatisfy` P.returns (P.just (P.gt 0))
        action `shouldNotSatisfy` P.returns (P.just (P.gt 10))

      it "shows helpful failure messages" $ do
        snapshotFailure (P.returns (P.left $ P.eq 0)) (pure $ Left 1)
        snapshotFailure (P.not $ P.returns (P.left $ P.eq 0)) (pure $ Left 0)

    describe "throws" $ do
      let throw404 = throwIO $ HttpException 404
      let exc code = P.con $ HttpException (P.eq code)

      it "checks exception" $ do
        throw404 `shouldSatisfy` P.throws (exc 404)
        throw404 `shouldNotSatisfy` P.throws (exc 500)

      it "shows helpful failure messages" $ do
        snapshotFailure (P.throws (exc 500)) throw404
        snapshotFailure (P.throws (exc 500)) (pure 1)
        snapshotFailure (P.not $ P.throws (exc 404)) throw404

snapshotFailure :: (HasCallStack) => Predicate a -> a -> IO ()
snapshotFailure p x = runPredicate p x `shouldSatisfy` P.returns (P.con $ PredicateFail P.matchesSnapshot)

normalizeConFailure :: String -> String
#if __GLASGOW_HASKELL__ == 906
normalizeConFailure = Text.unpack . Text.replace old new . Text.pack
  where
    old =
      Text.pack . unlines $
        [ "ExampleSpec.hs:9:3: error:"
        , "    • The constructor ‘User’ should have 2 arguments, but has been given 1"
        , "    • In a stmt of a 'do' block:"
        , "        User \"alice\" (Just 1)"
        , "          `shouldSatisfy`"
        , "            (((P.conMatches \"User\") Nothing)"
        , "               \\ actual"
        , "                 -> case pure actual of"
        , "                      Just (User x0)"
        , "                        -> Just"
        , "                             ((Skeletest.Internal.Utils.HList.HCons (pure x0))"
        , "                                Skeletest.Internal.Utils.HList.HNil)"
        , "                      _ -> Nothing)"
        , "              ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                 Skeletest.Internal.Utils.HList.HNil)"
        , "      In the second argument of ‘($)’, namely"
        , "        ‘do User \"alice\" (Just 1)"
        , "              `shouldSatisfy`"
        , "                (((P.conMatches \"User\") Nothing)"
        , "                   \\ actual"
        , "                     -> case pure actual of"
        , "                          Just (User x0) -> ..."
        , "                          _ -> ...)"
        , "                  ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                     Skeletest.Internal.Utils.HList.HNil)’"
        , "      In the expression:"
        , "        it \"should error\""
        , "          $ do User \"alice\" (Just 1)"
        , "                 `shouldSatisfy`"
        , "                   (((P.conMatches \"User\") Nothing)"
        , "                      \\ actual"
        , "                        -> case pure actual of"
        , "                             Just (User x0) -> ..."
        , "                             _ -> ...)"
        , "                     ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                        Skeletest.Internal.Utils.HList.HNil)"
        ]
    new =
      Text.pack . unlines $
        [ "ExampleSpec.hs:9:3: error: [GHC-27346]"
        , "    • The data constructor ‘User’ should have 2 arguments, but has been given 1"
        , "    • In the pattern: User x0"
        , "      In the pattern: Just (User x0)"
        , "      In a case alternative:"
        , "          Just (User x0)"
        , "            -> Just"
        , "                 ((Skeletest.Internal.Utils.HList.HCons (pure x0))"
        , "                    Skeletest.Internal.Utils.HList.HNil)"
        ]
#elif __GLASGOW_HASKELL__ == 908
normalizeConFailure = Text.unpack . Text.replace old new . Text.pack
  where
    old =
      Text.pack . unlines $
        [ "    • In a stmt of a 'do' block:"
        , "        User \"alice\" (Just 1)"
        , "          `shouldSatisfy`"
        , "            (((P.conMatches \"User\") Nothing)"
        , "               \\ actual"
        , "                 -> case pure actual of"
        , "                      Just (User x0)"
        , "                        -> Just"
        , "                             ((Skeletest.Internal.Utils.HList.HCons (pure x0))"
        , "                                Skeletest.Internal.Utils.HList.HNil)"
        , "                      _ -> Nothing)"
        , "              ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                 Skeletest.Internal.Utils.HList.HNil)"
        , "      In the second argument of ‘($)’, namely"
        , "        ‘do User \"alice\" (Just 1)"
        , "              `shouldSatisfy`"
        , "                (((P.conMatches \"User\") Nothing)"
        , "                   \\ actual"
        , "                     -> case pure actual of"
        , "                          Just (User x0) -> ..."
        , "                          _ -> ...)"
        , "                  ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                     Skeletest.Internal.Utils.HList.HNil)’"
        , "      In the expression:"
        , "        it \"should error\""
        , "          $ do User \"alice\" (Just 1)"
        , "                 `shouldSatisfy`"
        , "                   (((P.conMatches \"User\") Nothing)"
        , "                      \\ actual"
        , "                        -> case pure actual of"
        , "                             Just (User x0) -> ..."
        , "                             _ -> ...)"
        , "                     ((Skeletest.Internal.Utils.HList.HCons (P.eq \"\"))"
        , "                        Skeletest.Internal.Utils.HList.HNil)"
        ]
    new =
      Text.pack . unlines $
        [ "    • In the pattern: User x0"
        , "      In the pattern: Just (User x0)"
        , "      In a case alternative:"
        , "          Just (User x0)"
        , "            -> Just"
        , "                 ((Skeletest.Internal.Utils.HList.HCons (pure x0))"
        , "                    Skeletest.Internal.Utils.HList.HNil)"
        ]
#else
normalizeConFailure = Text.unpack . Text.pack
#endif
