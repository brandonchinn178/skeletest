{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.TestInfo (
  TestInfo (..),
  withTestInfo,
  getTestInfo,
  lookupTestInfo,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket_)

import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.Markers (SomeMarker)

data TestInfo = TestInfo
  { testModule :: Text
  , testContexts :: [Text]
  , testName :: Text
  , testMarkers :: [SomeMarker]
  , testFile :: FilePath
  -- ^ Relative to CWD
  }
  deriving (Show)

type TestInfoMap = Map ThreadId TestInfo

testInfoMapRef :: IORef TestInfoMap
testInfoMapRef = unsafePerformIO $ newIORef Map.empty
{-# NOINLINE testInfoMapRef #-}

withTestInfo :: TestInfo -> IO a -> IO a
withTestInfo info m = do
  tid <- myThreadId
  bracket_ (set tid) (unset tid) m
  where
    set tid = modifyIORef testInfoMapRef $ Map.insert tid info
    unset tid = modifyIORef testInfoMapRef $ Map.delete tid

lookupTestInfo :: IO (Maybe TestInfo)
lookupTestInfo = do
  tid <- myThreadId
  Map.lookup tid <$> readIORef testInfoMapRef

getTestInfo :: IO TestInfo
getTestInfo =
  lookupTestInfo >>= \case
    Just info -> pure info
    -- it's not possible for a user to write code that's executed within a test,
    -- because we define the entire main function.
    Nothing -> invariantViolation "test info not initialized"
