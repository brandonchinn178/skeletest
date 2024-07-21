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

  -- * Built-in fixtures
  FixtureTmpDir (..),
) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Ordered qualified as OMap
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Typeable (Typeable, eqT, typeOf, typeRep, (:~:) (Refl))
import System.Directory (createDirectory, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import UnliftIO.Exception (throwIO, tryAny)

import Skeletest.Internal.Error (SkeletestError (..), invariantViolation)
import Skeletest.Internal.State (
  FixtureCleanup (..),
  FixtureMap,
  FixtureRegistry (..),
  FixtureStatus (..),
  TestInfo (testFile),
  getTestInfo,
  modifyFixtureRegistry,
 )
import Skeletest.Internal.Utils.Map qualified as Map.Utils

class (Typeable a) => Fixture a where
  -- | The scope of the fixture, defaults to per-test
  fixtureScope :: FixtureScope
  fixtureScope = PerTestFixture

  fixtureAction :: IO (a, FixtureCleanup)

data FixtureScope
  = PerTestFixture
  | PerFileFixture
  | PerSessionFixture
  deriving (Show)

data FixtureScopeKey
  = PerTestFixtureKey ThreadId
  | PerFileFixtureKey FilePath
  | PerSessionFixtureKey
  deriving (Show)

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
        Nothing -> (insertFixture FixtureInProgress registry, Right Nothing)
        -- fixture has already been requested
        Just (FixtureLoaded (fixture :: ty, _)) ->
          case eqT @a @ty of
            Just Refl -> (registry, Right $ Just fixture)
            Nothing ->
              invariantViolation . unwords $
                [ "fixture registry contained incorrect types."
                , "Expected: " <> show rep <> "."
                , "Got: " <> show (typeOf fixture)
                ]
        Just FixtureInProgress ->
          -- get list of fixtures causing a circular dependency
          let fixtures = map fst . filter (isInProgress . snd) . OMap.assocs $ getScopedFixtures registry
           in (registry, Left $ FixtureCircularDependency $ map (Text.pack . show) (fixtures <> [rep]))

  case cachedFixture of
    -- error when getting fixture
    Left e -> throwIO e
    -- fixture was cached, return it
    Right (Just fixture) -> pure fixture
    -- otherwise, execute it (allowing it to request other fixtures) and cache the result.
    Right Nothing -> do
      result@(fixture, _) <- fixtureAction @a
      modifyFixtureRegistry $ \registry -> (insertFixture (FixtureLoaded result) registry, ())
      pure fixture
  where
    rep = typeRep (Proxy @a)
    isInProgress = \case
      FixtureInProgress -> True
      _ -> False

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
      FixtureInProgress -> pure Nothing
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

{----- Built-in fixtures -----}

-- | A fixture that provides a temporary directory that can be used in a test.
newtype FixtureTmpDir = FixtureTmpDir FilePath

instance Fixture FixtureTmpDir where
  fixtureAction = do
    tmpdir <- getTemporaryDirectory
    let dir = tmpdir </> "skeletest-tmp-dir"
    removePathForcibly dir
    createDirectory dir
    pure . withCleanup (FixtureTmpDir dir) $
      removePathForcibly dir
