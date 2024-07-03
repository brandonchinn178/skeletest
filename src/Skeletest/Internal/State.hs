{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.State (
  -- * Fixtures
  FixtureRegistry (..),
  FixtureStatus (..),
  FixtureCleanup (..),
  withFixtureRegistry,

  -- * CLI options
  -- TODO

  -- * Test info
  TestInfo (..),
  withTestInfo,
  getTestInfo,
  lookupTestInfo,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Typeable (Typeable, TypeRep)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket_)

-- | The global state shared by all of Skeletest.
data GlobalState = GlobalState
  { fixturesRegistry :: FixtureRegistry
  , cliOptions :: () -- TODO
  , testInfoMap :: Map ThreadId TestInfo
  }

globalStateRef :: IORef GlobalState
globalStateRef = unsafePerformIO $ newIORef newState
  where
    newState =
      GlobalState
        { fixturesRegistry = emptyFixtureRegistry
        , cliOptions = ()
        , testInfoMap = Map.empty
        }
{-# NOINLINE globalStateRef #-}

{----- Fixtures -----}

-- | The registry of active fixtures, in order of activation.
data FixtureRegistry = FixtureRegistry
  { sessionFixtures :: OMap TypeRep FixtureStatus
  , testFixtures :: OMap (TypeRep, ThreadId) FixtureStatus
  }

data FixtureStatus
  = FixtureInProgress
  | forall a. Typeable a => FixtureLoaded (a, FixtureCleanup)

data FixtureCleanup
  = NoCleanup
  | CleanupFunc (IO ())

emptyFixtureRegistry :: FixtureRegistry
emptyFixtureRegistry =
  FixtureRegistry
    { sessionFixtures = OMap.empty
    , testFixtures = OMap.empty
    }

withFixtureRegistry :: (FixtureRegistry -> (FixtureRegistry, a)) -> IO a
withFixtureRegistry f =
  atomicModifyIORef globalStateRef $ \s ->
    let (registry, a) = f (fixturesRegistry s)
     in (s{fixturesRegistry = registry}, a)

{----- Test info -----}

data TestInfo = TestInfo
  { testContexts :: [Text]
  , testName :: [Text]
  }

withTestInfo :: TestInfo -> IO a -> IO a
withTestInfo info m = do
  tid <- myThreadId
  bracket_ (set tid) (unset tid) m
  where
    set tid = modifyIORef globalStateRef (\s -> s{testInfoMap = Map.insert tid info (testInfoMap s)})
    unset tid = modifyIORef globalStateRef (\s -> s{testInfoMap = Map.delete tid (testInfoMap s)})

lookupTestInfo :: IO (Maybe TestInfo)
lookupTestInfo = do
  tid <- myThreadId
  Map.lookup tid . testInfoMap <$> readIORef globalStateRef

getTestInfo :: IO TestInfo
getTestInfo =
  lookupTestInfo >>= \case
    Just info -> pure info
    Nothing -> error "test info not initialized" -- TODO
