{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Snapshot (
  -- * Checking snapshot
  SnapshotContext (..),
  SnapshotResult (..),
  checkSnapshot,

  -- * Rendering
  SnapshotRenderer (..),
  defaultSnapshotRenderers,

  -- * Infrastructure
  SnapshotFixture (..),
  SnapshotUpdateFlag (..),
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.IORef (IORef, newIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Void (absurd)
import Debug.RecoverRTTI (anythingToString)
import System.FilePath (replaceExtension, splitFileName, (</>))
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (throwIO, try)

import Skeletest.Internal.CLI (IsFlag (..), FlagSpec (..))
import Skeletest.Internal.Fixtures (Fixture (..), noCleanup)
import Skeletest.Internal.State (TestInfo (..))

{----- Infrastructure -----}

data SnapshotFixture = SnapshotFixture
  { snapshotIndexRef :: IORef Int
  }

instance Fixture SnapshotFixture where
  fixtureAction = do
    snapshotIndexRef <- newIORef 0
    pure . noCleanup $ SnapshotFixture{..}

newtype SnapshotUpdateFlag = SnapshotUpdateFlag Bool

instance IsFlag SnapshotUpdateFlag where
  flagName = "update"
  flagShort = Just 'u'
  flagHelp = "Update snapshots"
  flagSpec = SwitchFlag SnapshotUpdateFlag

{----- Checking snapshot -----}

data SnapshotContext = SnapshotContext
  { snapshotRenderers :: [SnapshotRenderer]
  , snapshotTestInfo :: TestInfo
  , snapshotIndex :: Int
  }

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
    snapshotFileContents <-
      liftIO (try $ Text.readFile $ snapshotPath testFile) >>= \case
        Left e
          | isDoesNotExistError e -> returnE SnapshotMissing
          | otherwise -> throwIO e
        Right contents -> pure contents

    fileSnapshots <-
      case decodeSnapshotFile snapshotFileContents of
        Nothing -> error "corrupted snapshot file" -- TODO: better error
        Just SnapshotFile{snapshots} -> pure snapshots

    let snapshots = Map.findWithDefault [] (testContexts <> [testName]) fileSnapshots
    snapshotContent <- maybe (returnE SnapshotMissing) pure $ safeIndex snapshots snapshotIndex

    returnE $
      if snapshotContent == renderedTestResult
        then SnapshotMatches
        else SnapshotDiff{snapshotContent, renderedTestResult}
  where
    SnapshotContext
      { snapshotRenderers = renderers
      , snapshotTestInfo = TestInfo{testContexts, testName, testFile}
      , snapshotIndex
      } = snapshotContext

    returnE = throwE
    renderedTestResult = normalizeTrailingNewlines $ renderVal renderers testResult
    normalizeTrailingNewlines = (<> "\n") . Text.dropWhileEnd (== '\n')

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
  , snapshots :: Map [Text] [Text]
    -- ^ full test identifier => snapshots
    -- e.g. ["group1", "group2", "returns val1 and val2"] => ["val1", "val2"]
  }

snapshotPath :: FilePath -> FilePath
snapshotPath testFile = testDir </> "__snapshots__" </> snapshotFileName
  where
    (testDir, testFileName) = splitFileName testFile
    snapshotFileName = replaceExtension testFileName ".snap.md"

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

{----- Renderers -----}

-- TODO: Skeletest.Internal.Snapshot
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
  case mapMaybe tryRender renderers of
    [] -> Text.pack $ anythingToString a
    rendered : _ -> rendered
  where
    tryRender SnapshotRenderer{render} = render <$> Typeable.cast a
