{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Assertions (
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
  failTest,
  TestFailure (..),
) where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (CallStack, HasCallStack)
import GHC.Stack qualified as GHC
import UnliftIO.Exception (Exception (..), throwIO)

import Skeletest.Internal.Predicate (
  Predicate,
  PredicateResult (..),
  runPredicate,
 )
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.State (TestInfo, getTestInfo)

shouldBe :: (HasCallStack, Eq a) => a -> a -> IO ()
actual `shouldBe` expected = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.eq expected

shouldNotBe :: (HasCallStack, Eq a) => a -> a -> IO ()
actual `shouldNotBe` expected = GHC.withFrozenCallStack $ actual `shouldNotSatisfy` P.eq expected

shouldSatisfy :: (HasCallStack) => a -> Predicate a -> IO ()
actual `shouldSatisfy` p =
  GHC.withFrozenCallStack $
    runPredicate p actual >>= \case
      PredicateSuccess -> pure ()
      PredicateFail msg -> failTest' msg

shouldNotSatisfy :: (HasCallStack) => a -> Predicate a -> IO ()
actual `shouldNotSatisfy` p = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.not p

failTest :: (HasCallStack) => String -> IO a
failTest = GHC.withFrozenCallStack $ failTest' . Text.pack

failTest' :: (HasCallStack) => Text -> IO a
failTest' msg = do
  testInfo <- getTestInfo
  throwIO
    TestFailure
      { testInfo
      , testFailMessage = msg
      , callStack = GHC.callStack
      }

data TestFailure = TestFailure
  { testInfo :: TestInfo
  , testFailMessage :: Text
  , callStack :: CallStack
  }
  deriving (Show)

instance Exception TestFailure
