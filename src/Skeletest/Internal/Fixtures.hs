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

import Control.Concurrent (myThreadId)
import Data.Map.Ordered qualified as OMap
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, eqT, typeOf, typeRep, (:~:) (Refl))

import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.State (
  FixtureCleanup (..),
  FixtureRegistry (..),
  FixtureStatus (..),
  withFixtureRegistry,
 )

class Typeable a => Fixture a where
  fixtureDef :: FixtureDef a

data FixtureDef a = FixtureDef
  { fixtureScope :: FixtureScope
  , fixtureImpl :: IO (a, FixtureCleanup)
  }

data FixtureScope
  = PerTestFixture
  | PerSessionFixture

-- | A helper for defining the cleanup function in-line.
withCleanup :: a -> IO () -> (a, FixtureCleanup)
withCleanup a cleanup = (a, CleanupFunc cleanup)

-- | Load a fixture, initializing it if it hasn't been cached already.
getFixture :: forall a. Fixture a => IO a
getFixture = do
  tid <- myThreadId
  let (lookupFixture, setFixture) =
        case scope of
          PerTestFixture ->
            ( \registry -> OMap.lookup (rep, tid) (testFixtures registry)
            , \s registry -> registry{testFixtures = OMap.alter (const s) (rep, tid) (testFixtures registry)}
            )
          PerSessionFixture ->
            ( \registry -> OMap.lookup rep (sessionFixtures registry)
            , \s registry -> registry{sessionFixtures = OMap.alter (const s) rep (sessionFixtures registry)}
            )

  cachedFixture <-
    withFixtureRegistry $ \registry ->
      case lookupFixture registry of
        -- fixture has not been requested yet
        Nothing -> (setFixture (Just FixtureInProgress) registry, Nothing)
        -- fixture has already been requested
        Just (FixtureLoaded (fixture :: ty, _)) ->
          case eqT @a @ty of
            Just Refl -> (registry, Just fixture)
            Nothing ->
              invariantViolation . unwords $
                [ "loadedFixturesVar contained incorrect types."
                , "Expected: " <> show rep <> "."
                , "Got: " <> show (typeOf fixture)
                ]
        Just FixtureInProgress -> error "circular dependency" -- TODO: better error

  case cachedFixture of
    -- fixture was cached, return it
    Just fixture -> pure fixture
    -- otherwise, execute it (allowing it to request other fixtures) and cache the result.
    Nothing -> do
      result@(fixture, _) <- runFixture
      withFixtureRegistry $ \registry -> (setFixture (Just $ FixtureLoaded result) registry, ())
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
