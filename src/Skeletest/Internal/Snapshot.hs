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
  X.setSnapshotRenderers,
  X.getSnapshotRenderers,
  X.plainRenderer,
  X.renderWithShow,

  -- ** SnapshotFile
  SnapshotFile (..),
  SnapshotTestId,
  mkSnapshotTestId,
  SnapshotValue (..),
  decodeSnapshotFile,
  encodeSnapshotFile,
  normalizeSnapshotFile,

  -- * Plugin
  snapshotPlugin,
) where

import Control.Monad (guard, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Maybe qualified as Maybe
import Control.Monad.Trans.State.Strict qualified as State
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
import Skeletest.Internal.CLI qualified as CLI
import Skeletest.Internal.Error (skeletestError)
import Skeletest.Internal.Exit (TestExitCode (..))
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureScope (..),
  getFixture,
  noCleanup,
  withCleanup,
 )
import Skeletest.Internal.Hooks qualified as Hooks
import Skeletest.Internal.Paths (listTestFiles, readTestFile)
import Skeletest.Internal.Predicate (
  Predicate (..),
  PredicateFuncResult (..),
  ShowFailCtx (..),
 )
import Skeletest.Internal.Snapshot.Renderer (
  SnapshotRenderer (..),
  defaultSnapshotRenderers,
  getSnapshotRenderers,
 )
import Skeletest.Internal.Snapshot.Renderer qualified as X
import Skeletest.Internal.TestInfo (TestInfo (..), getTestInfo)
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Diff (showLineDiff)
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Internal.Utils.Text (pluralize, showT)
import Skeletest.Plugin (Hooks (..), Plugin (..), Spec, SpecInfo (..), SpecTest (..), SpecTree (..), TestResult (..), defaultHooks, defaultPlugin, getSpecTrees)
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

{----- Plugin -----}

snapshotPlugin :: Plugin
snapshotPlugin =
  defaultPlugin
    { hooks = snapshotsHook
    , cliFlags = [CLI.flag @SnapshotUpdateFlag]
    , snapshotRenderers = defaultSnapshotRenderers
    }

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
    { modifySpecRegistry = Hooks.mkPreHook_ $ \_ registry -> do
        -- Collect before the applyTestSelections hook to check for snapshots
        -- that don't correspond to any tests anymore
        modifyIORef' snapshotInfoStoreRef $ \store ->
          store
            { allSnapshotTestIds =
                Map.fromList
                  [ (getSnapshotPath specPath, getTestIds spec)
                  | SpecInfo{..} <- registry
                  ]
            }
        pure ()
    , runTest =
        Hooks.mkHook_
          ( \_ _ -> do
              SnapshotUpdateFlag isUpdate <- getFlag
              when isUpdate $ do
                -- Always initialize the file fixture to ensure snapshots get
                -- cleaned up for a test that removed all `P.matchesSnapshot`
                -- checks
                _ <- getFixture @UpdateSnapshotFixture_File
                pure ()
          )
          ( \ctx _ result -> do
              SnapshotUpdateFlag isUpdate <- getFlag
              when result.status.success $ do
                if isUpdate
                  then recordSnapshotsToFileFixture ctx.testInfo
                  else checkExtraTestSnapshots ctx.testInfo
              pure ()
          )
    , runSpecs = Hooks.mkPostHook $ \_ _ code -> do
        SnapshotUpdateFlag isUpdate <- getFlag
        if isUpdate
          then removeOutdatedSnapshots *> pure code
          else checkOutdatedSnapshots code
    , modifyTestSummary = Hooks.mkPreHook $ \_ summary -> do
        snapshotSummary <- getSnapshotSummary
        pure $ summary <> snapshotSummary
    }

-- | Snapshot-related information to store globally.
data SnapshotInfoStore = SnapshotInfoStore
  { allSnapshotTestIds :: !(Map FilePath [SnapshotTestId])
  -- ^ Map from a test file's snapshot path to all test ids in the file
  , snapshotFilesWithExtraSnapshots :: !(Set FilePath)
  -- ^ Snapshot files that contain tests that contain extraneous snapshots.
  , numSnapshotsUpdated :: !Int
  -- ^ Number of snapshots that were updated
  , numSnapshotFilesCleanedUp :: !Int
  -- ^ Number of snapshot files that were cleaned up
  }

-- | Map from "Test file's snapshot path" => "All test ids in the test file"
snapshotInfoStoreRef :: IORef SnapshotInfoStore
snapshotInfoStoreRef =
  unsafePerformIO . newIORef $
    SnapshotInfoStore
      { allSnapshotTestIds = Map.empty
      , snapshotFilesWithExtraSnapshots = Set.empty
      , numSnapshotsUpdated = 0
      , numSnapshotFilesCleanedUp = 0
      }
{-# NOINLINE snapshotInfoStoreRef #-}

getTestIds :: Spec -> [SnapshotTestId]
getTestIds = concatMap (go Seq.empty) . getSpecTrees
 where
  go context = \case
    group@SpecTree_Group{} -> concatMap (go (context Seq.|> group.label)) group.trees
    SpecTree_Test test -> [mkSnapshotTestId . Seq.toList $ context Seq.|> test.name]

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
        Nothing -> returnOutdated $ cleanupFile snapshotFilePath

    contents <- liftIO $ Text.readFile snapshotFilePath

    snapshotFile <-
      case decodeSnapshotFile contents of
        Just file -> pure file
        -- If Nothing, snapshot file is corrupted; we'll treat it the same as outdated.
        -- If this happens when '--update' is passed, it means no more tests in
        -- the file have snapshots, since it would've been regenerated. So just
        -- remove the snapshot file if we still encounter this.
        Nothing -> returnOutdated $ cleanupFile snapshotFilePath

    let outdatedSnapshots = Map.keysSet snapshotFile.snapshots Set.\\ testIds
    unless (null outdatedSnapshots) $
      returnOutdated $ do
        modifyIORef' snapshotInfoStoreRef $ \store ->
          store{numSnapshotsUpdated = store.numSnapshotsUpdated + length outdatedSnapshots}
        let snapshots' = Map.withoutKeys snapshotFile.snapshots outdatedSnapshots
        saveSnapshotFile snapshotFilePath snapshotFile{snapshots = snapshots'}

  cleanupFile path = do
    modifyIORef' snapshotInfoStoreRef $ \store ->
      store{numSnapshotFilesCleanedUp = store.numSnapshotFilesCleanedUp + 1}
    removeFile path

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
      mapM_ Term.output . concat $
        [ [""]
        , ["╓─ 🚨 " <> Color.bold "Outdated snapshots detected" <> " ────────────────"]
        , ["║  * " <> Text.pack fp | fp <- Set.toAscList outdated']
        , ["║"]
        , ["║  Update/remove these files with --update."]
        , ["╙─────────────────────────────────────────────────"]
        ]
      pure ExitOutdatedSnapshots

getSnapshotSummary :: IO Text
getSnapshotSummary = do
  store <- readIORef snapshotInfoStoreRef
  pure . Text.unlines . concat $
    [ when_ (store.numSnapshotsUpdated > 0) $
        "➤ " <> pluralize store.numSnapshotsUpdated "snapshot" <> " updated"
    , when_ (store.numSnapshotFilesCleanedUp > 0) $
        "➤ " <> pluralize store.numSnapshotFilesCleanedUp "snapshot file" <> " cleaned up"
    ]
 where
  when_ p x = if p then [x] else []

{----- Update snapshot -----}

-- | Collect snapshots for all tests in a file.
-- When test file is done, merge new snapshots into the existing snapshot file
-- and write to disk if it's changed.
data UpdateSnapshotFixture_File = UpdateSnapshotFixture_File
  { newFileSnapshotsRef :: IORef (Map SnapshotTestId [SnapshotValue])
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
  modifyIORef' newFileSnapshotsRef (Map.insert (getSnapshotTestId testInfo) newSnapshots)

finalizeUpdateSnapshotFixture :: TestInfo -> IORef (Map SnapshotTestId [SnapshotValue]) -> IO ()
finalizeUpdateSnapshotFixture testInfo newFileSnapshotsRef = do
  let snapshotPath = getSnapshotPath testInfo.file
  snapshotFile <-
    loadSnapshotFile snapshotPath >>= \case
      SnapshotFileLoadResult_Exists file -> pure file
      _ -> pure $ emptySnapshotFile (Text.pack testInfo.file)
  newSnapshots <- Map.map Seq.toList <$> readIORef newFileSnapshotsRef
  let snapshots' = mergeSnapshots snapshotFile.snapshots newSnapshots
  when (snapshots' /= snapshotFile.snapshots) $ do
    modifyIORef' snapshotInfoStoreRef $ \store ->
      store{numSnapshotsUpdated = store.numSnapshotsUpdated + countChanges snapshotFile.snapshots snapshots'}
    saveSnapshotFile snapshotPath snapshotFile{snapshots = snapshots'}
 where
  countChanges old new =
    flip State.execState 0 $
      Map.mergeA
        ( Map.traverseMissing $ \_ snaps -> do
            State.modify' (+ length snaps)
        )
        ( Map.traverseMissing $ \_ snaps -> do
            State.modify' (+ length snaps)
        )
        ( Map.zipWithAMatched $ \_ snapsOld snapsNew -> do
            let (snapsOld', snapsNew') = (Set.fromList snapsOld, Set.fromList snapsNew)
            let added = Set.size $ snapsNew' Set.\\ snapsOld'
            let removed = Set.size $ snapsOld' Set.\\ snapsNew'
            State.modify' (+ (added + removed))
        )
        old
        new

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
    mSnapshotFile <-
      loadSnapshotFile snapshotPath >>= \case
        SnapshotFileLoadResult_Exists file -> pure $ Just file
        SnapshotFileLoadResult_Missing -> pure Nothing
        SnapshotFileLoadResult_Corrupted -> skeletestError $ "Snapshot file was corrupted: " <> Text.pack snapshotPath
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
  let testSnapshots = Map.findWithDefault [] (getSnapshotTestId testInfo) snapshotFile.snapshots
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
    testSnapshots <- Maybe.hoistMaybe $ Map.lookup (getSnapshotTestId testInfo) snapshotFile.snapshots
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
  , snapshots :: Map SnapshotTestId [SnapshotValue]
  -- ^ Map from test identifier to its snapshots, e.g.
  -- "group1 ≫ group2 ≫ returns val1 and val2" => ["val1", "val2"]
  }
  deriving (Show, Eq)

newtype SnapshotTestId = SnapshotTestId Text
  deriving (Show, Eq, Ord)

data SnapshotValue = SnapshotValue
  { content :: Text
  , lang :: Maybe Text
  }
  deriving (Show, Eq, Ord)

mkSnapshotTestId :: [Text] -> SnapshotTestId
mkSnapshotTestId =
  SnapshotTestId
    . Text.intercalate " ≫ "
    . map (sanitizeNonPrint . sanitizeArrows . Text.strip)
 where
  sanitizeArrows = Text.replace "≫" ">>"

  -- Replace non-print characters with their escaped representations
  sanitizeNonPrint s =
    case Text.break (not . isPrint) s of
      (_, "") -> s -- quick exit in common case where text names are all printable chars
      (pre, post) -> pre <> Text.concatMap escapeChar post
   where
    escapeChar c =
      if isPrint c
        then Text.singleton c
        else Text.drop 1 . Text.dropEnd 1 . showT $ c

getSnapshotTestId :: TestInfo -> SnapshotTestId
getSnapshotTestId testInfo = mkSnapshotTestId $ testInfo.contexts <> [testInfo.name]

getSnapshotPath :: FilePath -> FilePath
getSnapshotPath testFile = stripDotSlash $ testDir </> "__snapshots__" </> snapshotFileName
 where
  (testDir, testFileName) = splitFileName testFile
  snapshotFileName = replaceExtension testFileName ".snap.md"
  stripDotSlash = \case
    '.' : '/' : dir -> dir
    dir -> dir

emptySnapshotFile :: Text -> SnapshotFile
emptySnapshotFile testFile =
  SnapshotFile
    { testFile
    , snapshots = Map.empty
    }

data SnapshotFileLoadResult
  = SnapshotFileLoadResult_Missing
  | SnapshotFileLoadResult_Corrupted
  | SnapshotFileLoadResult_Exists SnapshotFile

loadSnapshotFile :: FilePath -> IO SnapshotFileLoadResult
loadSnapshotFile path =
  handleDNE (\_ -> pure SnapshotFileLoadResult_Missing) $ do
    contents <- readTestFile path
    pure $
      case decodeSnapshotFile contents of
        Just file -> SnapshotFileLoadResult_Exists file
        Nothing -> SnapshotFileLoadResult_Corrupted
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
    Maybe SnapshotTestId -> -- The current test identifier, if one is set
    [Text] -> -- The rest of the lines to process
    Maybe SnapshotFile
  parseSections snapshotFile mTest = \case
    [] -> pure snapshotFile
    line : rest
      -- ignore empty lines
      | "" <- Text.strip line -> parseSections snapshotFile mTest rest
      -- found a test section
      | Just sectionName <- Text.stripPrefix "## " line -> do
          let testIdentifier
                -- Backwards compat, skeletest < 0.4 separated with "/"
                | not $ "≫" `Text.isInfixOf` sectionName = mkSnapshotTestId $ Text.splitOn " / " sectionName
                | otherwise = SnapshotTestId sectionName
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

encodeSnapshotFile :: (SnapshotTestId -> Int) -> SnapshotFile -> Text
encodeSnapshotFile rankTestId snapshotFile =
  Text.intercalate "\n" $ h1 snapshotFile.testFile : concatMap toSection snapshots
 where
  snapshots = sortOn (rankTestId . fst) . Map.toList $ snapshotFile.snapshots
  toSection (SnapshotTestId testId, snaps) = h2 testId : map codeBlock snaps

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
    { snapshots = map normalizeSnapshotVal <$> file.snapshots
    }

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
