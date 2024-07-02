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

import Data.Map.Ordered qualified as OMap
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, eqT, typeOf, typeRep, (:~:) (Refl))

import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.State (
  FixtureCleanup (..),
  FixtureRegistry (..),
  FixtureScope (..),
  FixtureState (..),
  FixtureStatus (..),
  withFixtureRegistry,
 )

class Typeable a => Fixture a where
  fixtureDef :: FixtureDef a

data FixtureDef a = FixtureDef
  { fixtureScope :: FixtureScope
  , fixtureImpl :: IO (a, FixtureCleanup)
  }

-- | A helper for defining the cleanup function in-line.
withCleanup :: a -> IO () -> (a, FixtureCleanup)
withCleanup a cleanup = (a, CleanupFunc cleanup)

-- | Load a fixture, initializing it if it hasn't been cached already.
getFixture :: forall a. Fixture a => IO a
getFixture = do
  cachedFixture <-
    withFixtureRegistry $ \(FixtureRegistry registry) ->
      case OMap.lookup rep registry of
        -- fixture has already been requested
        Just FixtureState{fixtureStatus} -> do
          case fixtureStatus of
            FixtureInProgress -> error "circular dependency" -- TODO: better error
            FixtureLoaded (fixture :: ty, _) ->
              case eqT @a @ty of
                Just Refl -> (FixtureRegistry registry, Just fixture)
                Nothing ->
                  invariantViolation . unwords $
                    [ "loadedFixturesVar contained incorrect types."
                    , "Expected: " <> show rep <> "."
                    , "Got: " <> show (typeOf fixture)
                    ]
        -- fixture has not been requested yet
        Nothing ->
          let initialState =
                FixtureState
                  { fixtureStateScope = scope
                  , fixtureStatus = FixtureInProgress @a
                  }
           in (FixtureRegistry $ registry OMap.|> (rep, initialState), Nothing)

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
      withFixtureRegistry $ \(FixtureRegistry registry) ->
        (FixtureRegistry $ OMap.alter (fmap updateState) rep registry, ())
      pure fixture
  where
    rep = typeRep (Proxy @a)
    FixtureDef{fixtureScope = scope, fixtureImpl = runFixture} = fixtureDef @a

-- | Clean up fixtures in the given scope.
--
-- TODO: iterate loadedFixturesVar with the given scope in reverse order
-- TODO: catch all errors and rethrow at end, to ensure everything is cleaned up.
cleanupFixtures :: FixtureScope -> IO ()
cleanupFixtures _ = pure ()
