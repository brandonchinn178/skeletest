{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Snapshot (
  -- * Running snapshot
  SnapshotContext (..),
  SnapshotResult (..),
  updateSnapshot,
  checkSnapshot,

  -- * Rendering
  SnapshotRenderer (..),
  defaultSnapshotRenderers,

  -- * Infrastructure
  getAndIncSnapshotIndex,
  SnapshotUpdateFlag (..),
) where

import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Void (absurd)
import Debug.RecoverRTTI (anythingToString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (replaceExtension, splitFileName, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (throwIO, try)
import UnliftIO.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)

import Skeletest.Internal.CLI (IsFlag (..), FlagSpec (..))
import Skeletest.Internal.Error (SkeletestError (..), invariantViolation)
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureScope (..),
  getFixture,
  noCleanup,
  withCleanup,
 )
import Skeletest.Internal.State (TestInfo (..), getTestInfo)
import Skeletest.Internal.Utils.Map qualified as Map.Utils

{----- Infrastructure -----}

data SnapshotTestFixture = SnapshotTestFixture
  { snapshotIndexRef :: IORef Int
  }

instance Fixture SnapshotTestFixture where
  fixtureAction = do
    snapshotIndexRef <- newIORef 0
    -- TODO: if --update, clean up extraneous snapshots when test finishes
    pure . noCleanup $ SnapshotTestFixture{..}

getAndIncSnapshotIndex :: IO Int
getAndIncSnapshotIndex = do
  SnapshotTestFixture{snapshotIndexRef} <- getFixture
  atomicModifyIORef' snapshotIndexRef $ \i -> (i + 1, i)

-- TODO: if all tests in file were run, error if snapshot file contains outdated tests (--update to remove)
data SnapshotFileFixture = SnapshotFileFixture
  { snapshotFileRef :: IORef (Maybe SnapshotFile)
  }

instance Fixture SnapshotFileFixture where
  fixtureScope = PerFileFixture
  fixtureAction = do
    TestInfo{testFile} <- getTestInfo
    let snapshotPath = getSnapshotPath testFile

    mSnapshotFile <-
      try (Text.readFile snapshotPath) >>= \case
        Left e
          | isDoesNotExistError e -> pure Nothing
          | otherwise -> throwIO e
        Right contents ->
          case decodeSnapshotFile contents of
            Just snapshotFile -> pure $ Just snapshotFile
            -- FIXME: add test
            Nothing -> throwIO $ SnapshotFileCorrupted snapshotPath
    let snapshotChanged newSnapshot = mSnapshotFile /= Just newSnapshot

    snapshotFileRef <- newIORef mSnapshotFile
    pure . withCleanup SnapshotFileFixture{..} $
      -- write snapshot back out when file is done
      readIORef snapshotFileRef >>= \case
        Just snapshotFile | snapshotChanged snapshotFile -> do
          createDirectoryIfMissing True (takeDirectory snapshotPath)
          Text.writeFile snapshotPath $ encodeSnapshotFile snapshotFile
        _ -> pure ()

-- TODO: statically analyze if P.matchesSnapshot appears anywhere in a test file
-- and check if there are snapshot files for test files that don't have snapshot
-- assertions. if --update, remove such files, or error if not --update.

newtype SnapshotUpdateFlag = SnapshotUpdateFlag Bool

instance IsFlag SnapshotUpdateFlag where
  flagName = "update"
  flagShort = Just 'u'
  flagHelp = "Update snapshots"
  flagSpec = SwitchFlag SnapshotUpdateFlag

{----- Running snapshot -----}

data SnapshotContext = SnapshotContext
  { snapshotRenderers :: [SnapshotRenderer]
  , snapshotTestInfo :: TestInfo
  , snapshotIndex :: Int
  }

updateSnapshot :: Typeable a => SnapshotContext -> a -> IO ()
updateSnapshot snapshotContext testResult = do
  SnapshotFileFixture{snapshotFileRef} <- getFixture
  modifyIORef' snapshotFileRef (Just . setSnapshot . fromMaybe emptySnapshotFile)
  where
    SnapshotContext
      { snapshotRenderers = renderers
      , snapshotTestInfo = testInfo@TestInfo{testModule}
      , snapshotIndex
      } = snapshotContext

    emptySnapshotFile =
      SnapshotFile
        { moduleName = testModule
        , snapshots = Map.empty
        }

    testIdentifier = toTestIdentifier testInfo
    renderedTestResult = renderVal renderers testResult
    setSnapshot snapshotFile@SnapshotFile{snapshots} =
      let setForTest = Map.Utils.adjustNested (setAt snapshotIndex renderedTestResult) testIdentifier
       in snapshotFile{snapshots = setForTest snapshots}

    -- Set the given snapshot at the given index. If the index is too large,
    -- fill in with empty snapshots.
    --
    -- >>> setAt 3 "x" ["a"] == ["a", "", "", "x"]
    setAt i0 v =
      let go = \cases
            i [] -> replicate i mempty <> [v]
            0 (_ : xs) -> v : xs
            i (x : xs) -> x : go (i - 1) xs
       in
        if i0 < 0
          then invariantViolation $ "Got negative snapshot index: " <> show i0
          else go i0

data SnapshotResult
  = SnapshotMissing
  | SnapshotMatches
  | SnapshotDiff
      { snapshotContent :: Text
      , renderedTestResult :: Text
      }
  deriving (Show, Eq)

-- TODO: use a per-file fixture to cache snapshot files and write it all back in cleanup?
checkSnapshot :: Typeable a => SnapshotContext -> a -> IO SnapshotResult
checkSnapshot snapshotContext testResult =
  fmap (either id absurd) . runExceptT $ do
    SnapshotFileFixture{snapshotFileRef} <- getFixture
    fileSnapshots <-
      readIORef snapshotFileRef >>= \case
        Nothing -> returnE SnapshotMissing
        Just SnapshotFile{snapshots} -> pure snapshots

    let snapshots = Map.Utils.findOrEmpty (toTestIdentifier testInfo) fileSnapshots
    snapshotContent <- maybe (returnE SnapshotMissing) pure $ safeIndex snapshots snapshotIndex

    returnE $
      if snapshotContent == renderedTestResult
        then SnapshotMatches
        else SnapshotDiff{snapshotContent, renderedTestResult}
  where
    SnapshotContext
      { snapshotRenderers = renderers
      , snapshotTestInfo = testInfo
      , snapshotIndex
      } = snapshotContext

    returnE = throwE
    renderedTestResult = renderVal renderers testResult

    safeIndex xs0 i0 =
      let go = \cases
            _ [] -> Nothing
            0 (x : _) -> Just x
            i (_ : xs) -> go (i - 1) xs
       in if i0 < 0 then Nothing else go i0 xs0

{----- Snapshot file -----}

-- TODO: keep ordered by test order?
data SnapshotFile = SnapshotFile
  { moduleName :: Text
  , snapshots :: Map TestIdentifier [Text]
    -- ^ full test identifier => snapshots
    -- e.g. ["group1", "group2", "returns val1 and val2"] => ["val1", "val2"]
  }
  deriving (Eq)

type TestIdentifier = [Text]

getSnapshotPath :: FilePath -> FilePath
getSnapshotPath testFile = testDir </> "__snapshots__" </> snapshotFileName
  where
    (testDir, testFileName) = splitFileName testFile
    snapshotFileName = replaceExtension testFileName ".snap.md"

toTestIdentifier :: TestInfo -> TestIdentifier
toTestIdentifier TestInfo{testContexts, testName} = testContexts <> [testName]

decodeSnapshotFile :: Text -> Maybe SnapshotFile
decodeSnapshotFile = parseFile . Text.lines
  where
    parseFile = \case
      line : rest
        | Just moduleName <- Text.stripPrefix "# " line -> do
            let snapshotFile =
                  SnapshotFile
                    { moduleName = Text.strip moduleName
                    , snapshots = Map.empty
                    }
            parseSections snapshotFile Nothing rest
      _ -> Nothing

    parseSections ::
      SnapshotFile -- ^ The parsed snapshot file so far
      -> Maybe [Text] -- ^ The current test identifier, if one is set
      -> [Text] -- ^ The rest of the lines to process
      -> Maybe SnapshotFile
    parseSections snapshotFile@SnapshotFile{snapshots} mTest = \case
      [] -> pure snapshotFile
      line : rest
        -- ignore empty lines
        | "" <- Text.strip line -> parseSections snapshotFile mTest rest
        -- found a test section
        | Just sectionName <- Text.stripPrefix "## " line -> do
            let testIdentifier = map Text.strip $ Text.splitOn "/" sectionName
            let snapshotFile' = snapshotFile{snapshots = Map.insert testIdentifier [] snapshots}
            parseSections snapshotFile' (Just testIdentifier) rest
        -- found the beginning of a snapshot
        | "```" <- Text.strip line -> do
            testIdentifier <- mTest
            (snapshot, rest') <- parseSnapshot [] rest
            let snapshotFile' = snapshotFile{snapshots = Map.adjust (<> [snapshot]) testIdentifier snapshots}
            parseSections snapshotFile' mTest rest'
        -- anything else is invalid
        | otherwise -> Nothing

    parseSnapshot snapshot = \case
      [] -> Nothing
      line : rest
        | "```" <- Text.strip line -> pure (Text.unlines snapshot, rest)
        | otherwise -> parseSnapshot (snapshot <> [line]) rest

-- FIXME: property test
encodeSnapshotFile :: SnapshotFile -> Text
encodeSnapshotFile SnapshotFile{..} =
  Text.intercalate "\n" $
    h1 moduleName : concatMap toSection (Map.toList snapshots)
  where
    toSection (testIdentifier, snaps) =
      h2 (Text.intercalate " / " testIdentifier) : map codeBlock snaps

    h1 s = "# " <> s <> "\n"
    h2 s = "## " <> s <> "\n"
    codeBlock s = "```\n" <> normalizeTrailingNewlines s <> "```\n"

{----- Renderers -----}

data SnapshotRenderer =
  forall a.
  Typeable a =>
  SnapshotRenderer
    { render :: a -> Text
    }

defaultSnapshotRenderers :: [SnapshotRenderer]
defaultSnapshotRenderers =
  [ SnapshotRenderer @String Text.pack
  , SnapshotRenderer @Text id
  ]

renderVal :: Typeable a => [SnapshotRenderer] -> a -> Text
renderVal renderers a =
  normalizeTrailingNewlines $
    case mapMaybe tryRender renderers of
      [] -> Text.pack $ anythingToString a
      rendered : _ -> rendered
  where
    tryRender SnapshotRenderer{render} = render <$> Typeable.cast a

-- | Ensure there's exactly one trailing newline.
normalizeTrailingNewlines :: Text -> Text
normalizeTrailingNewlines s = Text.dropWhileEnd (== '\n') s <> "\n"
