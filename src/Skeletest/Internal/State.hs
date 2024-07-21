{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.State (
  -- * Test info
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

-- | The global state shared by all of Skeletest.
data GlobalState = GlobalState
  { testInfoMap :: Map ThreadId TestInfo
  }

globalStateRef :: IORef GlobalState
globalStateRef = unsafePerformIO $ newIORef newState
  where
    newState =
      GlobalState
        { testInfoMap = Map.empty
        }
{-# NOINLINE globalStateRef #-}

{----- Test info -----}

data TestInfo = TestInfo
  { testModule :: Text
  , testContexts :: [Text]
  , testName :: Text
  , testMarkers :: [SomeMarker]
  , testFile :: FilePath
  -- ^ Relative to CWD
  }
  deriving (Show)

withTestInfo :: TestInfo -> IO a -> IO a
withTestInfo info m = do
  tid <- myThreadId
  bracket_ (set tid) (unset tid) m
  where
    set tid = modifyIORef globalStateRef $ \s -> s{testInfoMap = Map.insert tid info (testInfoMap s)}
    unset tid = modifyIORef globalStateRef $ \s -> s{testInfoMap = Map.delete tid (testInfoMap s)}

lookupTestInfo :: IO (Maybe TestInfo)
lookupTestInfo = do
  tid <- myThreadId
  Map.lookup tid . testInfoMap <$> readIORef globalStateRef

getTestInfo :: IO TestInfo
getTestInfo =
  lookupTestInfo >>= \case
    Just info -> pure info
    -- it's not possible for a user to write code that's executed within a test,
    -- because we define the entire main function.
    Nothing -> invariantViolation "test info not initialized"
