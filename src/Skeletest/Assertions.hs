{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Assertions (
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
  context,
  failTest,
  TestFailure (..),
) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (CallStack, HasCallStack)
import GHC.Stack qualified as GHC
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (Exception (..), bracket_, throwIO)

import Skeletest.Internal.Predicate (
  Predicate,
  PredicateResult (..),
  runPredicate,
 )
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.TestInfo (TestInfo, getTestInfo)

infix 1 `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldNotSatisfy`

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

context :: String -> IO a -> IO a
context msg =
  bracket_
    (modifyIORef failContextRef (Text.pack msg :))
    (modifyIORef failContextRef (drop 1))

failTest :: (HasCallStack) => String -> IO a
failTest = GHC.withFrozenCallStack $ failTest' . Text.pack

failTest' :: (HasCallStack) => Text -> IO a
failTest' msg = do
  testInfo <- getTestInfo
  ctx <- readIORef failContextRef
  throwIO
    TestFailure
      { testInfo
      , testFailMessage = msg
      , testFailContext = ctx
      , callStack = GHC.callStack
      }

data TestFailure = TestFailure
  { testInfo :: TestInfo
  , testFailMessage :: Text
  , testFailContext :: FailContext
  , callStack :: CallStack
  }
  deriving (Show)

instance Exception TestFailure

-- | Context for failures, in order of most recently added -> least recently added
type FailContext = [Text]

failContextRef :: IORef FailContext
failContextRef = unsafePerformIO $ newIORef []
{-# NOINLINE failContextRef #-}
