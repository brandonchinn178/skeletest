{-# LANGUAGE GADTs #-}

module Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureDef (..),
  FixtureScope (..),
  getFixture,

  -- * Cleanup
  FixtureCleanup (..),
  withCleanup,
  cleanupFixtures,
) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, TypeRep, eqT, typeOf, typeRep, (:~:) (Refl))
import System.IO.Unsafe (unsafePerformIO)

import Skeletest.Internal.Error (invariantViolation)

class Typeable a => Fixture a where
  fixtureDef :: FixtureDef a

data FixtureDef a = FixtureDef
  { fixtureScope :: FixtureScope
  , fixtureImpl :: IO (a, FixtureCleanup)
  }

data FixtureScope
  = PerTestFixture
  | PerSessionFixture

data FixtureCleanup
  = NoCleanup
  | CleanupFunc (IO ())

-- | A helper for defining the cleanup function in-line.
withCleanup :: a -> IO () -> (a, FixtureCleanup)
withCleanup a cleanup = (a, CleanupFunc cleanup)

-- | Load a fixture, initializing it if it hasn't been cached already.
getFixture :: forall a. Fixture a => IO a
getFixture = do
  cachedFixture <-
    modifyMVar loadedFixturesVar $ \loadedFixtures ->
      case OMap.lookup rep loadedFixtures of
        -- fixture has already been requested
        Just FixtureState{fixtureStatus} -> do
          case fixtureStatus of
            FixtureInProgress -> error "circular dependency" -- TODO: better error
            FixtureLoaded (fixture :: ty, _) ->
              case eqT @a @ty of
                Just Refl -> pure (loadedFixtures, Just fixture)
                Nothing ->
                  invariantViolation . unwords $
                    [ "loadedFixturesVar contained incorrect types."
                    , "Expected: " <> show rep <> "."
                    , "Got: " <> show (typeOf fixture)
                    ]
        -- fixture has not been requested yet
        Nothing -> do
          let initialState =
                FixtureState
                  { fixtureStateScope = scope
                  , fixtureStatus = FixtureInProgress @a
                  }
          pure (loadedFixtures OMap.|> (rep, initialState), Nothing)

  case cachedFixture of
    -- fixture was cached, return it
    Just fixture -> pure fixture
    -- otherwise, execute it (allowing it to request other fixtures) and cache the result.
    Nothing -> do
      (fixture, cleanup) <- runFixture
      let updateState state =
            state
              { fixtureStatus = FixtureLoaded (fixture, cleanup)
              }
      modifyMVar_ loadedFixturesVar (pure . OMap.alter (fmap updateState) rep)
      pure fixture
  where
    rep = typeRep (Proxy @a)
    FixtureDef{fixtureScope = scope, fixtureImpl = runFixture} = fixtureDef @a

-- | Clean up fixtures in the given scope.
--
-- TODO: iterate loadedFixturesVar with the given scope in reverse order
-- TODO: catch all errors and rethrow at end, to ensure everything is cleaned up.
cleanupFixtures :: FixtureScope -> IO ()
cleanupFixtures = undefined

-- | The registry of active fixtures, in order of activation.
--
-- TODO: check if this is thread safe, e.g. if we run tests in parallel
loadedFixturesVar :: MVar (OMap TypeRep FixtureState)
loadedFixturesVar = unsafePerformIO $ newMVar OMap.empty
{-# NOINLINE loadedFixturesVar #-}

data FixtureState =
  forall a.
  Fixture a =>
  FixtureState
    { fixtureStateScope :: FixtureScope
    , fixtureStatus :: FixtureStatus a
    }

data FixtureStatus a
  = FixtureInProgress
  | FixtureLoaded (a, FixtureCleanup)
