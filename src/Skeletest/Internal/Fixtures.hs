{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureScope (..),
  FixtureScopeKey (..),
  getFixture,

  -- * Cleanup
  FixtureCleanup (..),
  noCleanup,
  withCleanup,
  cleanupFixtures,
) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Ordered qualified as OMap
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, eqT, typeOf, typeRep, (:~:) (Refl))
import UnliftIO.Exception (throwIO, tryAny)

import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.State (
  FixtureCleanup (..),
  FixtureMap,
  FixtureRegistry (..),
  FixtureStatus (..),
  TestInfo (testFile),
  modifyFixtureRegistry,
  getTestInfo,
 )
import Skeletest.Internal.Utils.Map qualified as Map.Utils

class Typeable a => Fixture a where
  -- | The scope of the fixture, defaults to per-test
  fixtureScope :: FixtureScope
  fixtureScope = PerTestFixture

  fixtureAction :: IO (a, FixtureCleanup)

data FixtureScope
  = PerTestFixture
  | PerFileFixture
  | PerSessionFixture

data FixtureScopeKey
  = PerTestFixtureKey ThreadId
  | PerFileFixtureKey FilePath
  | PerSessionFixtureKey

-- | A helper for specifying no cleanup.
noCleanup :: a -> (a, FixtureCleanup)
noCleanup a = (a, NoCleanup)

-- | A helper for defining the cleanup function in-line.
withCleanup :: a -> IO () -> (a, FixtureCleanup)
withCleanup a cleanup = (a, CleanupFunc cleanup)

-- | Load a fixture, initializing it if it hasn't been cached already.
getFixture :: forall a m. (Fixture a, MonadIO m) => m a
getFixture = liftIO $ do
  (getScopedFixtures, updateScopedFixtures) <-
    fmap getScopedAccessors $
      case fixtureScope @a of
        PerTestFixture -> PerTestFixtureKey <$> myThreadId
        PerFileFixture -> PerFileFixtureKey . testFile <$> getTestInfo
        PerSessionFixture -> pure PerSessionFixtureKey

  let insertFixture state = updateScopedFixtures (OMap.>| (rep, state))

  cachedFixture <-
    modifyFixtureRegistry $ \registry ->
      case OMap.lookup rep $ getScopedFixtures registry of
        -- fixture has not been requested yet
        Nothing -> (insertFixture FixtureInProgress registry, Nothing)
        -- fixture has already been requested
        Just (FixtureLoaded (fixture :: ty, _)) ->
          case eqT @a @ty of
            Just Refl -> (registry, Just fixture)
            Nothing ->
              invariantViolation . unwords $
                [ "fixture registry contained incorrect types."
                , "Expected: " <> show rep <> "."
                , "Got: " <> show (typeOf fixture)
                ]
        Just FixtureInProgress -> error "circular dependency" -- FIXME: better error

  case cachedFixture of
    -- fixture was cached, return it
    Just fixture -> pure fixture
    -- otherwise, execute it (allowing it to request other fixtures) and cache the result.
    Nothing -> do
      result@(fixture, _) <- fixtureAction @a
      modifyFixtureRegistry $ \registry -> (insertFixture (FixtureLoaded result) registry, ())
      pure fixture
  where
    rep = typeRep (Proxy @a)

-- | Clean up fixtures in the given scope.
--
-- Clean up functions are run in the reverse order the fixtures finished in.
-- For example, if a test asks for fixtures A and C, A asks for B, and C asks
-- for D, the fixtures should finish loading in order: B, A, D, C.
-- Cleanup should then go in order: C, D, A, B.
cleanupFixtures :: FixtureScopeKey -> IO ()
cleanupFixtures scopeKey = do
  -- get fixtures in the given scope and clear
  fixtures <-
    modifyFixtureRegistry $ \registry ->
      (updateScopedFixtures (const OMap.empty) registry, getScopedFixtures registry)

  errors <-
    forM (reverse . map snd . OMap.assocs $ fixtures) $ \case
      FixtureInProgress -> invariantViolation "Fixture was unexpectedly in progress in cleanupFixtures"
      FixtureLoaded (_, NoCleanup) -> pure Nothing
      FixtureLoaded (_, CleanupFunc io) -> fromLeft <$> tryAny io

  -- throw the first error we encountered
  case catMaybes errors of
    e : _ -> throwIO e
    [] -> pure ()
  where
    (getScopedFixtures, updateScopedFixtures) = getScopedAccessors scopeKey
    fromLeft = \case
      Left x -> Just x
      Right _ -> Nothing

getScopedAccessors ::
  FixtureScopeKey
  -> ( FixtureRegistry -> FixtureMap
     , (FixtureMap -> FixtureMap) -> FixtureRegistry -> FixtureRegistry
     )
getScopedAccessors scopeKey =
  case scopeKey of
    PerTestFixtureKey tid ->
      ( Map.Utils.findOrEmpty tid . testFixtures
      , \f registry -> registry{testFixtures = Map.Utils.adjustNested f tid (testFixtures registry)}
      )
    PerFileFixtureKey fp ->
      ( Map.Utils.findOrEmpty fp . fileFixtures
      , \f registry -> registry{fileFixtures = Map.Utils.adjustNested f fp (fileFixtures registry)}
      )
    PerSessionFixtureKey ->
      ( sessionFixtures
      , \f registry -> registry{sessionFixtures = f (sessionFixtures registry)}
      )
