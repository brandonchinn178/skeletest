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
  TestFailure (..),

  -- * Testable
  Testable,
  runTestable,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as GHC
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket_, throwIO)

import Skeletest.Internal.Predicate (
  Predicate,
  PredicateResult (..),
  runPredicate,
 )
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.TestInfo (getTestInfo)
import Skeletest.Internal.Testable (FailContext, TestFailure (..), Testable (..))

instance Testable IO where
  runTestable = id
  context = contextIO

infix 1 `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldNotSatisfy`

shouldBe :: (HasCallStack, Testable m, Eq a) => a -> a -> m ()
actual `shouldBe` expected = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.eq expected

shouldNotBe :: (HasCallStack, Testable m, Eq a) => a -> a -> m ()
actual `shouldNotBe` expected = GHC.withFrozenCallStack $ actual `shouldNotSatisfy` P.eq expected

-- TODO: work in both IO and Property
shouldSatisfy :: (HasCallStack, Testable m) => a -> Predicate a -> m ()
actual `shouldSatisfy` p =
  GHC.withFrozenCallStack $
    liftIO $
      runPredicate p actual >>= \case
        PredicateSuccess -> pure ()
        PredicateFail msg -> failTest' msg

shouldNotSatisfy :: (HasCallStack, Testable m) => a -> Predicate a -> m ()
actual `shouldNotSatisfy` p = GHC.withFrozenCallStack $ actual `shouldSatisfy` P.not p

contextIO :: String -> IO a -> IO a
contextIO msg =
  bracket_
    (modifyIORef failContextRef (Text.pack msg :))
    (modifyIORef failContextRef (drop 1))

failTest :: (HasCallStack, Testable m) => String -> m a
failTest = GHC.withFrozenCallStack $ liftIO . failTest' . Text.pack

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

failContextRef :: IORef FailContext
failContextRef = unsafePerformIO $ newIORef []
{-# NOINLINE failContextRef #-}
