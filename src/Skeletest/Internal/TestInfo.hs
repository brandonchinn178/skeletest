{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.TestInfo (
  TestInfo (..),
  TestId,
  withTestInfo,
  getTestInfo,
  lookupTestInfo,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.Markers (SomeMarker)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (ThreadId, myThreadId)
import UnliftIO.Exception (bracket_)
import UnliftIO.IORef (IORef, modifyIORef, newIORef, readIORef)

data TestInfo = TestInfo
  { contexts :: [Text]
  , name :: Text
  , markers :: [SomeMarker]
  , file :: FilePath
  -- ^ Relative to CWD
  }
  deriving (Show)

type TestId = [Text]
instance HasField "testId" TestInfo TestId where
  getField testInfo = testInfo.contexts <> [testInfo.name]

type TestInfoMap = Map ThreadId TestInfo

testInfoMapRef :: IORef TestInfoMap
testInfoMapRef = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE testInfoMapRef #-}

withTestInfo :: (MonadUnliftIO m) => TestInfo -> m a -> m a
withTestInfo info m = do
  tid <- myThreadId
  bracket_ (set tid) (unset tid) m
 where
  set tid = modifyIORef testInfoMapRef $ Map.insert tid info
  unset tid = modifyIORef testInfoMapRef $ Map.delete tid

lookupTestInfo :: (MonadIO m) => m (Maybe TestInfo)
lookupTestInfo = do
  tid <- myThreadId
  Map.lookup tid <$> readIORef testInfoMapRef

getTestInfo :: (MonadIO m, HasCallStack) => m TestInfo
getTestInfo =
  lookupTestInfo >>= \case
    Just info -> pure info
    -- it's not possible for a user to write code that's executed within a test,
    -- because we define the entire main function.
    Nothing -> invariantViolation "test info not initialized"
