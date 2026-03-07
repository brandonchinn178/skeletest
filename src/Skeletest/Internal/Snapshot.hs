{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Snapshot (
  -- * Running snapshot
  SnapshotResult (..),
  checkSnapshot,

  -- * Rendering
  SnapshotRenderer (..),
  defaultSnapshotRenderers,
  setSnapshotRenderers,
  getSnapshotRenderers,
  plainRenderer,
  renderWithShow,

  -- ** SnapshotFile
  SnapshotFile (..),
  SnapshotValue (..),
  decodeSnapshotFile,
  encodeSnapshotFile,
  normalizeSnapshotFile,

  -- * Infrastructure
  SnapshotUpdateFlag (..),
) where

import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except qualified as Except
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Char (isAlpha, isPrint)
import Data.Foldable qualified as Seq (toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Merge.Strict qualified as Map.Merge
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Void (absurd)
import Debug.RecoverRTTI (anythingToString)
import Skeletest.Internal.CLI (FlagSpec (..), IsFlag (..), getFlag)
import Skeletest.Internal.Error (skeletestError)
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureScope (..),
  getFixture,
  withCleanup,
 )
import Skeletest.Internal.Paths (readTestFile)
import Skeletest.Internal.TestInfo (TestId, TestInfo (..), getTestInfo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (replaceExtension, splitFileName, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (handleJust)
import UnliftIO.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )

{----- Infrastructure -----}

data SnapshotChecker = SnapshotChecker (forall a. (Typeable a) => a -> IO SnapshotResult)

instance Fixture SnapshotChecker where
  fixtureScope = PerFileFixture
  fixtureAction = do
    testInfo <- getTestInfo
    let snapshotPath = getSnapshotPath testInfo.file

    SnapshotUpdateFlag doUpdate <- getFlag
    (checker, onCleanup) <-
      if doUpdate
        then initSnapshotChecker_Update (Text.pack testInfo.file) snapshotPath
        else initSnapshotChecker_Check snapshotPath

    pure $ withCleanup checker onCleanup

newtype SnapshotUpdateFlag = SnapshotUpdateFlag Bool

instance IsFlag SnapshotUpdateFlag where
  flagName = "update"
  flagShort = Just 'u'
  flagHelp = "Update snapshots"
  flagSpec = SwitchFlag SnapshotUpdateFlag

{----- Running snapshot -----}

data SnapshotResult
  = SnapshotMissing
  | SnapshotMatches
  | SnapshotDiff
      { snapshotContent :: Text
      , renderedTestResult :: Text
      }
  deriving (Show, Eq)

checkSnapshot :: (Typeable a, MonadIO m) => a -> m SnapshotResult
checkSnapshot actual = do
  SnapshotChecker check <- getFixture
  liftIO $ check actual

initSnapshotChecker_Update :: Text -> FilePath -> IO (SnapshotChecker, IO ())
initSnapshotChecker_Update testFile snapshotPath = do
  newSnapshotsRef <- newIORef Map.empty
  renderers <- getSnapshotRenderers

  let checker = SnapshotChecker $ \val -> do
        testId <- (.testId) <$> getTestInfo
        let newSnapshotVal = renderVal renderers val
        modifyIORef' newSnapshotsRef $ \newSnapshots ->
          Map.alter
            (Just . (Seq.|> newSnapshotVal) . fromMaybe Seq.empty)
            testId
            newSnapshots
        pure SnapshotMatches

  let onCleanup = do
        snapshotFile <- fromMaybe (emptySnapshotFile testFile) <$> loadSnapshotFile snapshotPath
        newSnapshots <- Map.map Seq.toList <$> readIORef newSnapshotsRef
        let updatedSnapshots = mergeSnapshots snapshotFile.snapshots newSnapshots
        when (updatedSnapshots /= snapshotFile.snapshots) $ do
          createDirectoryIfMissing True (takeDirectory snapshotPath)
          Text.writeFile snapshotPath . encodeSnapshotFile . normalizeSnapshotFile $
            snapshotFile{snapshots = updatedSnapshots}

  pure (checker, onCleanup)
 where
  -- TODO: Clean up outdated snapshots in file (#24)
  mergeSnapshots old new =
    Map.Merge.merge
      Map.Merge.preserveMissing
      Map.Merge.preserveMissing
      (Map.Merge.zipWithMatched mergeSnapshotVals)
      old
      new
  mergeSnapshotVals _ old new =
    -- If test has extra snapshots, keep them, in case the test failed and didn't
    -- make it to all the snapshot assertions.
    -- TODO: Don't save when test fails (#25)
    -- TODO: Clean up outdated snapshots in test (#24)
    new <> drop (length new) old

initSnapshotChecker_Check :: FilePath -> IO (SnapshotChecker, IO ())
initSnapshotChecker_Check snapshotPath = do
  mSnapshotFile <- loadSnapshotFile snapshotPath
  renderers <- getSnapshotRenderers
  snapshotIndexesRef <- newIORef Map.empty

  let checker = SnapshotChecker $ \val -> runReturnE $ do
        testId <- (.testId) <$> getTestInfo

        index <- atomicModifyIORef' snapshotIndexesRef $ \snapshotIndexes ->
          let index = Map.findWithDefault 0 testId snapshotIndexes
           in (Map.insert testId (index + 1) snapshotIndexes, index)

        snapshotFile <- maybe (returnE SnapshotMissing) pure mSnapshotFile
        let testSnapshots = Map.findWithDefault [] testId snapshotFile.snapshots
        snapshot <-
          maybe (returnE SnapshotMissing) (pure . NonEmpty.head) $
            (NonEmpty.nonEmpty . drop index) testSnapshots

        let newSnapshotVal = renderVal renderers val
        returnE $
          if snapshot.content == newSnapshotVal.content
            then SnapshotMatches
            else
              SnapshotDiff
                { snapshotContent = snapshot.content
                , renderedTestResult = newSnapshotVal.content
                }

  let onCleanup = pure ()

  pure (checker, onCleanup)
 where
  runReturnE = fmap (either id absurd) . Except.runExceptT
  returnE = Except.throwE

loadSnapshotFile :: FilePath -> IO (Maybe SnapshotFile)
loadSnapshotFile path =
  handleDNE (\_ -> pure Nothing) . fmap Just $ do
    contents <- readTestFile path
    case decodeSnapshotFile contents of
      Just file -> pure file
      Nothing -> skeletestError $ "Snapshot file was corrupted: " <> Text.pack path
 where
  handleDNE = handleJust (\e -> guard (isDoesNotExistError e) *> Just e)

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
getSnapshotPath testFile = testDir </> "__snapshots__" </> snapshotFileName
 where
  (testDir, testFileName) = splitFileName testFile
  snapshotFileName = replaceExtension testFileName ".snap.md"

emptySnapshotFile :: Text -> SnapshotFile
emptySnapshotFile testFile =
  SnapshotFile
    { testFile
    , snapshots = Map.empty
    }

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
      | Just lang <- (Text.stripPrefix "```" . Text.strip) line -> do
          testIdentifier <- mTest
          (snapshot, rest') <- parseSnapshot [] rest
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
      | "```" <- Text.strip line -> pure (Text.unlines snapshot, rest)
      | otherwise -> parseSnapshot (snapshot <> [line]) rest

encodeSnapshotFile :: SnapshotFile -> Text
encodeSnapshotFile snapshotFile =
  Text.intercalate "\n" $
    h1 snapshotFile.testFile : concatMap toSection (Map.toList snapshotFile.snapshots)
 where
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

{----- Renderers -----}

data SnapshotRenderer
  = forall a.
  (Typeable a) =>
  SnapshotRenderer
  { render :: a -> Text
  , snapshotLang :: Maybe Text
  }

plainRenderer :: (Typeable a) => (a -> Text) -> SnapshotRenderer
plainRenderer render =
  SnapshotRenderer
    { render
    , snapshotLang = Nothing
    }

renderWithShow :: forall a. (Typeable a, Show a) => SnapshotRenderer
renderWithShow = plainRenderer (Text.pack . show @a)

defaultSnapshotRenderers :: [SnapshotRenderer]
defaultSnapshotRenderers =
  [ plainRenderer @String Text.pack
  , plainRenderer @Text id
  , jsonRenderer
  ]
 where
  jsonRenderer =
    SnapshotRenderer
      { render = TextL.toStrict . TextL.decodeUtf8 . Aeson.encodePretty @Aeson.Value
      , snapshotLang = Just "json"
      }

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
    { content = normalizeTrailingNewlines snapshot.content
    , lang = collapse $ Text.filter isAlpha <$> snapshot.lang
    }
 where
  collapse = \case
    Just "" -> Nothing
    m -> m

  -- Ensure there's exactly one trailing newline.
  normalizeTrailingNewlines s = Text.dropWhileEnd (== '\n') s <> "\n"

snapshotRenderersRef :: IORef [SnapshotRenderer]
snapshotRenderersRef = unsafePerformIO $ newIORef []
{-# NOINLINE snapshotRenderersRef #-}

setSnapshotRenderers :: [SnapshotRenderer] -> IO ()
setSnapshotRenderers = writeIORef snapshotRenderersRef

getSnapshotRenderers :: (MonadIO m) => m [SnapshotRenderer]
getSnapshotRenderers = readIORef snapshotRenderersRef
