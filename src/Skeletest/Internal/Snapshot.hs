{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Snapshot (
  -- * Predicate
  matchesSnapshot,

  -- * Rendering
  X.SnapshotRenderer (..),
  X.defaultSnapshotRenderers,
  X.setSnapshotRenderers,
  X.getSnapshotRenderers,
  X.plainRenderer,
  X.renderWithShow,

  -- ** SnapshotFile
  SnapshotFile (..),
  SnapshotValue (..),
  decodeSnapshotFile,
  encodeSnapshotFile,
  normalizeSnapshotFile,

  -- * Infrastructure
  SnapshotUpdateFlag (..),
  snapshotsHook,
) where

import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Maybe qualified as Maybe
import Data.Char (isAlpha, isPrint)
import Data.Foldable qualified as Seq (toList)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Void (absurd)
import Debug.RecoverRTTI (anythingToString)
import Skeletest.Internal.CLI (FlagSpec (..), IsFlag (..), getFlag)
import Skeletest.Internal.Error (skeletestError)
import Skeletest.Internal.Exit (TestExitCode (..))
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureScope (..),
  getFixture,
  noCleanup,
  withCleanup,
 )
import Skeletest.Internal.Paths (listTestFiles, readTestFile)
import Skeletest.Internal.Predicate (
  Predicate (..),
  PredicateFuncResult (..),
  ShowFailCtx (..),
 )
import Skeletest.Internal.Snapshot.Renderer (
  SnapshotRenderer (..),
  getSnapshotRenderers,
 )
import Skeletest.Internal.Snapshot.Renderer qualified as X
import Skeletest.Internal.TestInfo (TestId, TestInfo (..), getTestInfo)
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Diff (showLineDiff)
import Skeletest.Plugin (
  Hooks (..),
  Spec,
  SpecInfo (..),
  SpecTest (..),
  SpecTree (..),
  TestResult (..),
  defaultHooks,
  getSpecTrees,
 )
import System.FilePath (
  replaceExtension,
  splitFileName,
  takeDirectory,
  takeExtensions,
  (</>),
 )
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Directory (createDirectoryIfMissing, removeFile)
import UnliftIO.Exception (handleJust)
import UnliftIO.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
 )

-- | A predicate checking if the input matches the snapshot.
-- See the "Snapshot tests" section in the README.
--
-- >>> user `shouldSatisfy` P.matchesSnapshot
matchesSnapshot :: (Typeable a, MonadIO m) => Predicate m a
matchesSnapshot =
  Predicate
    { predicateFunc = \actual -> do
        SnapshotUpdateFlag doUpdate <- getFlag
        SnapshotChecker check <-
          if doUpdate
            then (.checker) <$> getFixture @UpdateSnapshotFixture
            else (.checker) <$> getFixture @CheckSnapshotFixture
        result <- liftIO $ check actual
        pure
          PredicateFuncResult
            { predicateSuccess = result == SnapshotMatches
            , predicateExplain =
                Text.intercalate "\n" $
                  case result of
                    SnapshotMissing renderedVal ->
                      [ "Snapshot does not exist. Update snapshot with --update."
                      , showLineDiff ("expected", "") ("actual", renderedVal)
                      ]
                    SnapshotMatches ->
                      [ "Matches snapshot"
                      ]
                    SnapshotDiff snapshot renderedActual ->
                      [ "Result differed from snapshot. Update snapshot with --update."
                      , showLineDiff ("expected", snapshot) ("actual", renderedActual)
                      ]
            , predicateShowFailCtx = HideFailCtx
            }
    , predicateDisp = "matches snapshot"
    , predicateDispNeg = "does not match snapshot"
    }

{----- Infrastructure -----}

newtype SnapshotUpdateFlag = SnapshotUpdateFlag Bool

instance IsFlag SnapshotUpdateFlag where
  flagName = "update"
  flagShort = Just 'u'
  flagHelp = "Update snapshots"
  flagSpec = SwitchFlag SnapshotUpdateFlag

data SnapshotChecker = SnapshotChecker (forall a. (Typeable a) => a -> IO SnapshotResult)

data SnapshotResult
  = SnapshotMissing
      { renderedVal :: Text
      }
  | SnapshotMatches
  | SnapshotDiff
      { snapshotContent :: Text
      , renderedTestResult :: Text
      }
  deriving (Show, Eq)

snapshotsHook :: Hooks
snapshotsHook =
  defaultHooks
    { modifySpecRegistry = \_ modify registry -> do
        -- Collect before the applyTestSelections hook to check for snapshots
        -- that don't correspond to any tests anymore
        modifyIORef' snapshotInfoStoreRef $ \store ->
          store
            { allSnapshotTestIds =
                Map.fromList
                  [ (getSnapshotPath specPath, getTestIds specSpec)
                  | SpecInfo{..} <- registry
                  ]
            }
        modify registry
    , runTest = \testInfo getResult -> do
        SnapshotUpdateFlag isUpdate <- getFlag
        when isUpdate $ do
          -- Always initialize the file fixture to ensure snapshots get
          -- cleaned up for a test that removed all `P.matchesSnapshot`
          -- checks
          _ <- getFixture @UpdateSnapshotFixture_File
          pure ()
        result <- getResult
        when result.testResultSuccess $ do
          if isUpdate
            then recordSnapshotsToFileFixture testInfo
            else checkExtraTestSnapshots testInfo
        pure result
    , runSpecs = \run specs -> do
        SnapshotUpdateFlag isUpdate <- getFlag
        code <- run specs
        if isUpdate
          then removeOutdatedSnapshots *> pure code
          else checkOutdatedSnapshots code
    }

-- | Snapshot-related information to store globally.
data SnapshotInfoStore = SnapshotInfoStore
  { allSnapshotTestIds :: Map FilePath [TestId]
  -- ^ Map from a test file's snapshot path to all test ids in the file
  , snapshotFilesWithExtraSnapshots :: Set FilePath
  -- ^ Snapshot files that contain tests that contain extraneous snapshots.
  }

-- | Map from "Test file's snapshot path" => "All test ids in the test file"
snapshotInfoStoreRef :: IORef SnapshotInfoStore
snapshotInfoStoreRef =
  unsafePerformIO . newIORef $
    SnapshotInfoStore
      { allSnapshotTestIds = Map.empty
      , snapshotFilesWithExtraSnapshots = Set.empty
      }
{-# NOINLINE snapshotInfoStoreRef #-}

getTestIds :: Spec -> [TestId]
getTestIds = concatMap (go Seq.empty) . getSpecTrees
 where
  go context = \case
    group@SpecTree_Group{} -> concatMap (go (context Seq.|> group.label)) group.trees
    SpecTree_Test test -> [Seq.toList $ context Seq.|> test.name]

-- | Detect outdated snapshots, returning the filepath to the outdated
-- snapshot and the action to clean it up.
detectOutdatedSnapshots :: IO [(FilePath, IO ())]
detectOutdatedSnapshots = do
  allSnapshotFiles <- filter isSnapshotFile <$> listTestFiles
  store <- readIORef snapshotInfoStoreRef
  let allTests = Map.map Set.fromList store.allSnapshotTestIds
  mapMaybeM (detectOutdated allTests) allSnapshotFiles
 where
  isSnapshotFile fp = takeExtensions fp == ".snap.md"
  mapMaybeM f = fmap catMaybes . mapM f

  detectOutdated allTests = runDetectOutdatedM $ \snapshotFilePath -> do
    testIds <-
      case Map.lookup snapshotFilePath allTests of
        Just testIds -> pure testIds
        -- If Nothing, snapshot file does not correspond to any tests
        Nothing -> returnOutdated $ removeFile snapshotFilePath

    contents <- liftIO $ Text.readFile snapshotFilePath

    snapshotFile <-
      case decodeSnapshotFile contents of
        Just file -> pure file
        -- If Nothing, snapshot file is corrupted; we'll treat it the same as outdated.
        -- If this happens when '--update' is passed, it means no more tests in
        -- the file have snapshots, since it would've been regenerated. So just
        -- remove the snapshot file if we still encounter this.
        Nothing -> returnOutdated $ removeFile snapshotFilePath

    let outdatedSnapshots = Map.keysSet snapshotFile.snapshots Set.\\ testIds
    unless (null outdatedSnapshots) $
      returnOutdated $ do
        let snapshots' = Map.withoutKeys snapshotFile.snapshots outdatedSnapshots
        saveSnapshotFile snapshotFilePath snapshotFile{snapshots = snapshots'}

  runDetectOutdatedM ::
    (FilePath -> Except.ExceptT (IO ()) IO ()) ->
    FilePath ->
    IO (Maybe (FilePath, IO ()))
  runDetectOutdatedM action fp =
    either (\io -> Just (fp, io)) (\_ -> Nothing)
      <$> Except.runExceptT (action fp)
  returnOutdated = Except.throwE

removeOutdatedSnapshots :: IO ()
removeOutdatedSnapshots = mapM_ snd =<< detectOutdatedSnapshots

checkOutdatedSnapshots :: TestExitCode -> IO TestExitCode
checkOutdatedSnapshots code = do
  outdated <- map fst <$> detectOutdatedSnapshots
  store <- readIORef snapshotInfoStoreRef
  let outdated' = Set.fromList outdated <> store.snapshotFilesWithExtraSnapshots
  if Set.null outdated'
    then pure code
    else do
      mapM_ Text.putStrLn . concat $
        [ [""]
        , ["╓─ 🚨 " <> Color.bold "Outdated snapshots detected" <> " ────────────────"]
        , ["║  * " <> Text.pack fp | fp <- Set.toAscList outdated']
        , ["║"]
        , ["║  Update/remove these files with --update."]
        , ["╙─────────────────────────────────────────────────"]
        ]
      pure ExitOutdatedSnapshots

{----- Update snapshot -----}

-- | Collect snapshots for all tests in a file.
-- When test file is done, merge new snapshots into the existing snapshot file
-- and write to disk if it's changed.
data UpdateSnapshotFixture_File = UpdateSnapshotFixture_File
  { newFileSnapshotsRef :: IORef (Map TestId [SnapshotValue])
  }

instance Fixture UpdateSnapshotFixture_File where
  fixtureScope = PerFileFixture
  fixtureAction = do
    testInfo <- getTestInfo
    newFileSnapshotsRef <- newIORef Map.empty
    pure . withCleanup UpdateSnapshotFixture_File{newFileSnapshotsRef} $ do
      finalizeUpdateSnapshotFixture testInfo newFileSnapshotsRef

data UpdateSnapshotFixture = UpdateSnapshotFixture
  { checker :: SnapshotChecker
  , newSnapshotsRef :: IORef (Seq SnapshotValue)
  }

instance Fixture UpdateSnapshotFixture where
  fixtureScope = PerTestFixture
  fixtureAction = do
    newSnapshotsRef <- newIORef Seq.empty
    let checker = SnapshotChecker (recordSnapshot newSnapshotsRef)
    pure $ noCleanup UpdateSnapshotFixture{checker, newSnapshotsRef}

-- | Collect `P.matchesSnapshot` results into a list per test.
recordSnapshot :: (Typeable a) => IORef (Seq SnapshotValue) -> a -> IO SnapshotResult
recordSnapshot newSnapshotsRef val = do
  renderers <- getSnapshotRenderers
  let newSnapshotVal = renderVal renderers val
  modifyIORef' newSnapshotsRef (Seq.|> newSnapshotVal)
  pure SnapshotMatches

-- | Copy snapshots to the file fixture when test is over.
recordSnapshotsToFileFixture :: TestInfo -> IO ()
recordSnapshotsToFileFixture testInfo = do
  UpdateSnapshotFixture_File{newFileSnapshotsRef} <- getFixture
  UpdateSnapshotFixture{newSnapshotsRef} <- getFixture
  newSnapshots <- Seq.toList <$> readIORef newSnapshotsRef
  modifyIORef' newFileSnapshotsRef (Map.insert testInfo.testId newSnapshots)

finalizeUpdateSnapshotFixture :: TestInfo -> IORef (Map TestId [SnapshotValue]) -> IO ()
finalizeUpdateSnapshotFixture testInfo newFileSnapshotsRef = do
  let snapshotPath = getSnapshotPath testInfo.file
  snapshotFile <- fromMaybe newSnapshotFile <$> loadSnapshotFile snapshotPath
  newSnapshots <- Map.map Seq.toList <$> readIORef newFileSnapshotsRef
  let snapshots' = mergeSnapshots snapshotFile.snapshots newSnapshots
  when (snapshots' /= snapshotFile.snapshots) $ do
    saveSnapshotFile snapshotPath snapshotFile{snapshots = snapshots'}
 where
  newSnapshotFile = emptySnapshotFile (Text.pack testInfo.file)

  -- Merge snapshots, to avoid clearing snapshots of tests that were deselected.
  -- Extraneous snapshots will be cleared by 'detectOutdatedSnapshots'.
  mergeSnapshots old new =
    Map.filter (not . null) $
      Map.merge
        Map.preserveMissing -- Keep snapshots for tests that weren't run
        Map.preserveMissing -- Add new snapshots
        (Map.zipWithMatched $ \_ _o n -> n) -- Overwrite old snapshots
        old
        new

{----- Check snapshot -----}

data CheckSnapshotFixture_File = CheckSnapshotFixture_File
  { mSnapshotFile :: Maybe SnapshotFile
  }

instance Fixture CheckSnapshotFixture_File where
  fixtureScope = PerFileFixture
  fixtureAction = do
    testFile <- (.file) <$> getTestInfo
    let snapshotPath = getSnapshotPath testFile
    mSnapshotFile <- loadSnapshotFile snapshotPath
    pure $ noCleanup CheckSnapshotFixture_File{mSnapshotFile}

data CheckSnapshotFixture = CheckSnapshotFixture
  { checker :: SnapshotChecker
  , snapshotIndexRef :: IORef Int
  }

instance Fixture CheckSnapshotFixture where
  fixtureScope = PerTestFixture
  fixtureAction = do
    testInfo <- getTestInfo
    snapshotIndexRef <- newIORef 0
    let checker = SnapshotChecker (runCheckSnapshot testInfo snapshotIndexRef)
    pure $ noCleanup CheckSnapshotFixture{checker, snapshotIndexRef}

runCheckSnapshot :: (Typeable a) => TestInfo -> IORef Int -> a -> IO SnapshotResult
runCheckSnapshot testInfo snapshotIndexRef val = runReturnE $ do
  CheckSnapshotFixture_File{mSnapshotFile} <- getFixture
  renderers <- getSnapshotRenderers

  let newSnapshotVal = renderVal renderers val
      snapshotMissing = SnapshotMissing newSnapshotVal.content

  index <- atomicModifyIORef' snapshotIndexRef $ \index -> (index + 1, index)

  snapshotFile <- maybe (returnE snapshotMissing) pure mSnapshotFile
  let testSnapshots = Map.findWithDefault [] testInfo.testId snapshotFile.snapshots
  snapshot <-
    maybe (returnE snapshotMissing) (pure . NonEmpty.head) $
      (NonEmpty.nonEmpty . drop index) testSnapshots

  returnE $
    if snapshot.content == newSnapshotVal.content
      then SnapshotMatches
      else
        SnapshotDiff
          { snapshotContent = snapshot.content
          , renderedTestResult = newSnapshotVal.content
          }
 where
  runReturnE = fmap (either id absurd) . Except.runExceptT
  returnE = Except.throwE

-- | Check if the snapshot file contains any extra snapshots for the current test
checkExtraTestSnapshots :: TestInfo -> IO ()
checkExtraTestSnapshots testInfo = do
  CheckSnapshotFixture_File{mSnapshotFile} <- getFixture
  fmap (fromMaybe ()) . Maybe.runMaybeT $ do
    snapshotFile <- Maybe.hoistMaybe mSnapshotFile
    testSnapshots <- Maybe.hoistMaybe $ Map.lookup testInfo.testId snapshotFile.snapshots
    CheckSnapshotFixture{snapshotIndexRef} <- getFixture
    index <- readIORef snapshotIndexRef
    when (length testSnapshots > index) $ do
      let snapshotPath = getSnapshotPath $ Text.unpack snapshotFile.testFile
      modifyIORef' snapshotInfoStoreRef $ \store ->
        store
          { snapshotFilesWithExtraSnapshots =
              Set.insert snapshotPath store.snapshotFilesWithExtraSnapshots
          }

{----- Snapshot file -----}

data SnapshotFile = SnapshotFile
  { testFile :: Text
  , snapshots :: Map TestId [SnapshotValue]
  -- ^ full test identifier => snapshots
  -- e.g. ["group1", "group2", "returns val1 and val2"] => ["val1", "val2"]
  }
  deriving (Show, Eq)

data SnapshotValue = SnapshotValue
  { content :: Text
  , lang :: Maybe Text
  }
  deriving (Show, Eq)

getSnapshotPath :: FilePath -> FilePath
getSnapshotPath testFile = testDir' </> "__snapshots__" </> snapshotFileName
 where
  (testDir, testFileName) = splitFileName testFile
  testDir' = if testDir == "./" then "" else testDir
  snapshotFileName = replaceExtension testFileName ".snap.md"

emptySnapshotFile :: Text -> SnapshotFile
emptySnapshotFile testFile =
  SnapshotFile
    { testFile
    , snapshots = Map.empty
    }

loadSnapshotFile :: FilePath -> IO (Maybe SnapshotFile)
loadSnapshotFile path =
  handleDNE (\_ -> pure Nothing) . fmap Just $ do
    contents <- readTestFile path
    case decodeSnapshotFile contents of
      Just file -> pure file
      Nothing -> skeletestError $ "Snapshot file was corrupted: " <> Text.pack path
 where
  handleDNE = handleJust (\e -> guard (isDoesNotExistError e) *> Just e)

saveSnapshotFile :: FilePath -> SnapshotFile -> IO ()
saveSnapshotFile path snapshotFile =
  if Map.null snapshotFile.snapshots
    then removeFile path
    else do
      rankTestId <- mkRankTestId <$> readIORef snapshotInfoStoreRef
      createDirectoryIfMissing True (takeDirectory path)
      Text.writeFile path . encodeSnapshotFile rankTestId . normalizeSnapshotFile $
        snapshotFile
 where
  mkRankTestId store =
    let testIds = Map.findWithDefault [] path store.allSnapshotTestIds
        testIdToRank = Map.fromList $ zip testIds [0 ..]
     in \testId ->
          Map.findWithDefault
            (maxBound @Int) -- Shouldn't happen, but just in case
            testId
            testIdToRank

decodeSnapshotFile :: Text -> Maybe SnapshotFile
decodeSnapshotFile = parseFile . Text.lines
 where
  parseFile = \case
    line : rest
      | Just testFile <- Text.stripPrefix "# " line -> do
          let snapshotFile =
                SnapshotFile
                  { testFile = Text.strip testFile
                  , snapshots = Map.empty
                  }
          parseSections snapshotFile Nothing rest
    _ -> Nothing

  parseSections ::
    SnapshotFile -> -- The parsed snapshot file so far
    Maybe [Text] -> -- The current test identifier, if one is set
    [Text] -> -- The rest of the lines to process
    Maybe SnapshotFile
  parseSections snapshotFile mTest = \case
    [] -> pure snapshotFile
    line : rest
      -- ignore empty lines
      | "" <- Text.strip line -> parseSections snapshotFile mTest rest
      -- found a test section
      | Just sectionName <- Text.stripPrefix "## " line -> do
          let testIdentifier = map Text.strip $ Text.splitOn " / " sectionName
          let snapshotFile' = snapshotFile{snapshots = Map.insert testIdentifier [] snapshotFile.snapshots}
          parseSections snapshotFile' (Just testIdentifier) rest
      -- found the beginning of a snapshot
      | Just lang <- Text.stripPrefix "```" line -> do
          testIdentifier <- mTest
          (snapshot, rest') <- parseSnapshot Seq.empty rest
          let
            snapshotVal =
              SnapshotValue
                { content = snapshot
                , lang = if Text.null lang then Nothing else Just lang
                }
            snapshotFile' = snapshotFile{snapshots = Map.adjust (<> [snapshotVal]) testIdentifier snapshotFile.snapshots}
          parseSections snapshotFile' mTest rest'
      -- anything else is invalid
      | otherwise -> Nothing

  parseSnapshot snapshot = \case
    [] -> Nothing
    line : rest
      | "```" <- line -> pure (Text.unlines $ Seq.toList snapshot, rest)
      | otherwise -> parseSnapshot (snapshot Seq.|> line) rest

encodeSnapshotFile :: (TestId -> Int) -> SnapshotFile -> Text
encodeSnapshotFile rankTestId snapshotFile =
  Text.intercalate "\n" $ h1 snapshotFile.testFile : concatMap toSection snapshots
 where
  snapshots = sortOn (rankTestId . fst) . Map.toList $ snapshotFile.snapshots
  toSection (testIdentifier, snaps) =
    h2 (Text.intercalate " / " testIdentifier) : map codeBlock snaps

  h1 s = "# " <> s <> "\n"
  h2 s = "## " <> s <> "\n"
  codeBlock snapshot =
    Text.concat
      [ "```" <> fromMaybe "" snapshot.lang <> "\n"
      , snapshot.content
      , "```\n"
      ]

normalizeSnapshotFile :: SnapshotFile -> SnapshotFile
normalizeSnapshotFile file =
  file
    { snapshots = Map.fromList . map normalize . Map.toList $ file.snapshots
    }
 where
  normalize (testIdentifier, vals) =
    ( map (sanitizeNonPrint . sanitizeSlashes . Text.strip) testIdentifier
    , map normalizeSnapshotVal vals
    )

  sanitizeSlashes = Text.replace " /" " \\/"

  sanitizeNonPrint = Text.concatMap $ \case
    c | (not . isPrint) c -> Text.drop 1 . Text.dropEnd 1 . Text.pack . show $ c
    c -> Text.singleton c

{----- Render values -----}

renderVal :: (Typeable a) => [SnapshotRenderer] -> a -> SnapshotValue
renderVal renderers a =
  normalizeSnapshotVal $
    case mapMaybe tryRender renderers of
      [] ->
        SnapshotValue
          { content = Text.pack $ anythingToString a
          , lang = Nothing
          }
      rendered : _ -> rendered
 where
  tryRender renderer@SnapshotRenderer{render} =
    let toValue v = SnapshotValue{content = render v, lang = renderer.snapshotLang}
     in toValue <$> Typeable.cast a

normalizeSnapshotVal :: SnapshotValue -> SnapshotValue
normalizeSnapshotVal snapshot =
  SnapshotValue
    { content =
        normalizeTrailingNewlines
          . sanitizeBackTicks
          $ snapshot.content
    , lang = collapse $ Text.filter isAlpha <$> snapshot.lang
    }
 where
  collapse = \case
    Just "" -> Nothing
    m -> m

  sanitizeBackTicks = Text.replace "```" "\\`\\`\\`"
  -- Ensure there's exactly one trailing newline.
  normalizeTrailingNewlines s = Text.dropWhileEnd (== '\n') s <> "\n"
