{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.State (
  -- * Fixtures
  FixtureRegistry (..),
  FixtureMap,
  FixtureStatus (..),
  FixtureCleanup (..),
  modifyFixtureRegistry,

  -- * CLI options
  CLIFlagStore,
  setCliFlagStore,
  lookupCliFlag,

  -- * Test info
  TestInfo (..),
  withTestInfo,
  getTestInfo,
  lookupTestInfo,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Data.Dynamic (Dynamic)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import Data.Typeable (Typeable, TypeRep)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (bracket_)

import Skeletest.Internal.Markers (SomeMarker)

-- | The global state shared by all of Skeletest.
data GlobalState = GlobalState
  { fixturesRegistry :: FixtureRegistry
  , cliFlags :: CLIFlagStore
  , testInfoMap :: Map ThreadId TestInfo
  }

globalStateRef :: IORef GlobalState
globalStateRef = unsafePerformIO $ newIORef newState
  where
    newState =
      GlobalState
        { fixturesRegistry = emptyFixtureRegistry
        , cliFlags = Map.empty
        , testInfoMap = Map.empty
        }
{-# NOINLINE globalStateRef #-}

{----- Fixtures -----}

-- | The registry of active fixtures, in order of activation.
data FixtureRegistry = FixtureRegistry
  { sessionFixtures :: FixtureMap
  , fileFixtures :: Map FilePath FixtureMap
  , testFixtures :: Map ThreadId FixtureMap
  }

type FixtureMap = OMap TypeRep FixtureStatus

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
    , fileFixtures = Map.empty
    , testFixtures = Map.empty
    }

modifyFixtureRegistry :: (FixtureRegistry -> (FixtureRegistry, a)) -> IO a
modifyFixtureRegistry f =
  atomicModifyIORef globalStateRef $ \s ->
    let (registry, a) = f (fixturesRegistry s)
     in (s{fixturesRegistry = registry}, a)

{----- CLI flags -----}

type CLIFlagStore = Map TypeRep Dynamic

setCliFlagStore :: CLIFlagStore -> IO ()
setCliFlagStore flags = modifyIORef globalStateRef $ \s -> s{cliFlags = flags}

lookupCliFlag :: TypeRep -> IO (Maybe Dynamic)
lookupCliFlag rep = Map.lookup rep . cliFlags <$> readIORef globalStateRef

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
    Nothing -> error "test info not initialized" -- FIXME: better error
