{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Skeletest.Assertions (
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
  context,
  failTest,
  AssertionFail (..),

  -- * Testable
  Testable,
  runTestable,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as GHC
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket_, throwIO)
import UnliftIO.IORef (IORef, modifyIORef, newIORef, readIORef)

import Skeletest.Internal.Predicate (
  Predicate,
  PredicateResult (..),
  runPredicate,
 )
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.TestInfo (getTestInfo)
import Skeletest.Internal.TestRunner (
  AssertionFail (..),
  FailContext,
  Testable (..),
  testResultPass,
 )

instance Testable IO where
  runTestable m = m >> pure testResultPass
  context = contextIO
  throwFailure = throwIO

infix 1 `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldNotSatisfy`

-- | Assert that the given input should match the given value.
-- Equivalent to @actual `shouldSatisfy` P.eq expected@
shouldBe :: (HasCallStack, Testable m, Eq a) => a -> a -> m ()
actual `shouldBe` expected = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.eq expected

-- | Assert that the given input should not match the given value.
-- Equivalent to @actual `shouldNotSatisfy` P.eq expected@
shouldNotBe :: (HasCallStack, Testable m, Eq a) => a -> a -> m ()
actual `shouldNotBe` expected = GHC.withFrozenCallStack $ actual `shouldNotSatisfy` P.eq expected

-- | Assert that the given input should satisfy the given predicate.
shouldSatisfy :: (HasCallStack, Testable m) => a -> Predicate m a -> m ()
actual `shouldSatisfy` p =
  GHC.withFrozenCallStack $
    runPredicate p actual >>= \case
      PredicateSuccess -> pure ()
      PredicateFail msg -> failTest' msg

-- | Assert that the given input should not satisfy the given predicate.
shouldNotSatisfy :: (HasCallStack, Testable m) => a -> Predicate m a -> m ()
actual `shouldNotSatisfy` p = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.not p

contextIO :: String -> IO a -> IO a
contextIO msg =
  bracket_
    (modifyIORef failContextRef (Text.pack msg :))
    (modifyIORef failContextRef (drop 1))

-- | Unconditionally fail the test with the given message.
failTest :: (HasCallStack, Testable m) => String -> m a
failTest = GHC.withFrozenCallStack $ failTest' . Text.pack

failTest' :: (HasCallStack, Testable m) => Text -> m a
failTest' msg = do
  testInfo <- getTestInfo
  ctx <- readIORef failContextRef
  throwFailure
    AssertionFail
      { testInfo
      , testFailMessage = msg
      , testFailContext = ctx
      , callStack = GHC.callStack
      }

failContextRef :: IORef FailContext
failContextRef = unsafePerformIO $ newIORef []
{-# NOINLINE failContextRef #-}
