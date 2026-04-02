{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Spec.TestReporter (
  TestReporter,
  newTestReporter,
) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import GHC.Records (HasField (..))
import Skeletest.Internal.CLI (FormatFlag (..), getFormatFlag)
import Skeletest.Internal.Exit (TestExitCode (..))
import Skeletest.Internal.Spec.Output (BoxSpec, BoxSpecContent (..))
import Skeletest.Internal.TestInfo (TestInfo (..))
import Skeletest.Internal.TestRunner (TestResult (..), TestResultMessage (..))
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Internal.Utils.Text (indentWith)
import Skeletest.Internal.Utils.Timer (renderDuration)
import UnliftIO.Async (Async)
import UnliftIO.Async qualified as Async
import UnliftIO.MVar (MVar, modifyMVar_, newMVar)

{----- TestReporter -----}

data TestReporter = TestReporter
  { format :: FormatFlag
  , supportsANSI :: Bool
  , animationThread :: AnimationThread
  , minimalFormatFailures :: IORef (Set FilePath)
  }

newTestReporter :: IO TestReporter
newTestReporter = do
  format <- getFormatFlag
  supportsANSI <- Term.supportsANSI Term.stdout
  animationThread <- newAnimationThread
  minimalFormatFailures <- newIORef Set.empty
  pure TestReporter{format, supportsANSI, animationThread, minimalFormatFailures}

getFormatAction :: forall field a. (HasField field FormatActions (TestReporter -> a)) => TestReporter -> a
getFormatAction reporter = getField @field formatActions reporter
 where
  formatActions =
    case reporter.format of
      FormatFlag_Minimal -> formatActionsMinimal
      FormatFlag_Full -> formatActionsFull
      FormatFlag_Verbose -> formatActionsVerbose

instance HasField "reportFilePre" TestReporter (FilePath -> IO ()) where
  getField = getFormatAction @"reportFilePre"
instance HasField "reportFilePost" TestReporter (FilePath -> (TestExitCode, NominalDiffTime) -> IO ()) where
  getField = getFormatAction @"reportFilePost"
instance HasField "reportGroupPre" TestReporter (TestInfo -> Text -> IO ()) where
  getField = getFormatAction @"reportGroupPre"
instance HasField "reportGroupPost" TestReporter (TestInfo -> Text -> (TestExitCode, NominalDiffTime) -> IO ()) where
  getField = getFormatAction @"reportGroupPost"
instance HasField "reportTestPre" TestReporter (TestInfo -> IO ()) where
  getField = getFormatAction @"reportTestPre"
instance HasField "reportTestPost" TestReporter (TestInfo -> (TestResult, NominalDiffTime) -> IO ()) where
  getField = getFormatAction @"reportTestPost"

{----- Report formats -----}

data FormatActions = FormatActions
  { reportFilePre :: TestReporter -> FilePath -> IO ()
  , reportFilePost :: TestReporter -> FilePath -> (TestExitCode, NominalDiffTime) -> IO ()
  , reportGroupPre :: TestReporter -> TestInfo -> Text -> IO ()
  , reportGroupPost :: TestReporter -> TestInfo -> Text -> (TestExitCode, NominalDiffTime) -> IO ()
  , reportTestPre :: TestReporter -> TestInfo -> IO ()
  , reportTestPost :: TestReporter -> TestInfo -> (TestResult, NominalDiffTime) -> IO ()
  }

defaultFormatActions :: FormatActions
defaultFormatActions =
  FormatActions
    { reportFilePre = \_ _ -> pure ()
    , reportFilePost = \_ _ _ -> pure ()
    , reportGroupPre = \_ _ _ -> pure ()
    , reportGroupPost = \_ _ _ _ -> pure ()
    , reportTestPre = \_ _ -> pure ()
    , reportTestPost = \_ _ _ -> pure ()
    }

formatActionsMinimal :: FormatActions
formatActionsMinimal =
  defaultFormatActions
    { reportFilePre
    , reportFilePost
    , reportTestPre
    , reportTestPost
    }
 where
  reportFilePre reporter fp = do
    when (not reporter.supportsANSI) $ do
      Term.outputN $ renderFile fp

  reportFilePost reporter fp (_, duration) = do
    fileHadFailure <-
      atomicModifyIORef' reporter.minimalFormatFailures $ \failures ->
        (Set.delete fp failures, fp `Set.member` failures)
    when (not fileHadFailure) $ do
      Term.output . Text.concat $
        [ if reporter.supportsANSI then renderFile fp else ""
        , Color.green "OK"
        , renderDurationLabel reporter duration
        ]

  renderFile fp = "◈ " <> Text.pack fp <> ": "

  reportTestPre reporter testInfo = do
    when reporter.supportsANSI $ do
      reporter.animationThread.start
        Animation
          { fps = 5
          , render = \t -> spinner t <> " " <> minimalTestLabel reporter testInfo
          }
   where
    spinner = mkAnimation $ map Color.yellow ["▶ ▷ ▷", "▷ ▶ ▷", "▷ ▷ ▶"]

  reportTestPost reporter testInfo (result, duration) = do
    when reporter.supportsANSI $ do
      reporter.animationThread.clear
    when (not result.testResultSuccess) $ do
      hadPreviousFailure <-
        atomicModifyIORef' reporter.minimalFormatFailures $ \failures ->
          (Set.insert testInfo.file failures, testInfo.file `Set.member` failures)
      if reporter.supportsANSI
        then do
          Term.outputN "◈ "
        else do
          when (not hadPreviousFailure) $ Term.outputN "\n"
          Term.outputN $ drawBoxHeader 1 (BoxHeaderType_Inline "")
      Term.output . Text.concat $
        [ minimalTestLabel reporter testInfo
        , ": "
        , result.testResultLabel
        , renderDurationLabel reporter duration
        ]
      renderTestResultMessage 0 result

  minimalTestLabel reporter testInfo =
    Text.intercalate " ≫ " . concat $
      [ if reporter.supportsANSI then [Text.pack testInfo.file] else []
      , testInfo.contexts
      , [testInfo.name]
      ]

formatActionsFull :: FormatActions
formatActionsFull =
  defaultFormatActions
    { reportFilePre
    , reportGroupPre
    , reportTestPre
    , reportTestPost
    }
 where
  reportFilePre _ fp = do
    Term.output $ Text.pack fp

  reportGroupPre _ testInfo name = do
    Term.output $ fullIndent indentLevel name
   where
    indentLevel = getIndentLevel testInfo

  reportTestPre reporter testInfo = do
    let testLabel = getTestLabel testInfo
    if reporter.supportsANSI
      then do
        reporter.animationThread.start
          Animation
            { fps = 12
            , render = \t -> testLabel <> spinner t
            }
      else do
        Term.outputN testLabel
   where
    spinner = mkAnimation $ map Color.yellow ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

  reportTestPost reporter testInfo (result, duration) = do
    when reporter.supportsANSI $ do
      reporter.animationThread.clear
      Term.outputInPlace $ getTestLabel testInfo
    withBoxHeader $ do
      Term.output $ result.testResultLabel <> durationLabel
    renderTestResultMessage indentLevel result
   where
    indentLevel = getIndentLevel testInfo
    isBox = \case
      TestResultMessageBox _ -> True
      _ -> False
    withBoxHeader action
      | not $ isBox result.testResultMessage = do
          action
      | reporter.supportsANSI = do
          Term.outputInPlace $ drawBoxHeader indentLevel (BoxHeaderType_Inline $ testInfo.name <> ": ")
          action
      | otherwise = do
          action
          Term.output $ drawBoxHeader indentLevel BoxHeaderType_NextLine
    durationLabel = renderDurationLabel reporter duration

  getTestLabel testInfo = fullIndent (getIndentLevel testInfo) (testInfo.name <> ": ")

-- Verbose is the same as full, except with some minor changes, so
-- we'll re-use full and inspect format directly
formatActionsVerbose :: FormatActions
formatActionsVerbose = formatActionsFull

renderDurationLabel :: TestReporter -> NominalDiffTime -> Text
renderDurationLabel reporter duration =
  if reporter.format == FormatFlag_Verbose || duration > 0.1
    then " " <> Color.gray ("(" <> renderDuration duration <> ")")
    else ""

renderTestResultMessage :: IndentLevel -> TestResult -> IO ()
renderTestResultMessage indentLevel result =
  case result.testResultMessage of
    TestResultMessageNone -> pure ()
    TestResultMessageInline msg -> do
      Term.output $ fullIndent (indentLevel + 1) msg
    TestResultMessageBox box -> do
      Term.outputN $ drawBoxBody box
      Term.output drawBoxFooter

{----- BoxSpec -----}

data BoxHeaderType
  = -- | e.g.
    --   "╭── my test: FAIL"
    BoxHeaderType_Inline Text
  | -- | e.g.
    --   "    my test: FAIL"
    --   "╭───╯"
    BoxHeaderType_NextLine

drawBoxHeader :: IndentLevel -> BoxHeaderType -> Text
drawBoxHeader lvl type_ = "╭" <> dashes <> suffix
 where
  dashes = Text.replicate (4 * lvl - 2) "─"
  suffix =
    case type_ of
      BoxHeaderType_Inline s -> " " <> s
      BoxHeaderType_NextLine -> "─╯"

drawBoxBody :: BoxSpec -> Text
drawBoxBody boxContents = Text.unlines $ concatMap draw boxContents
 where
  draw = \case
    BoxHeader s ->
      [ "│"
      , "╞═══ " <> s
      ]
    BoxText s ->
      [ "│ " <> line
      | line <- Text.lines s
      ]

drawBoxFooter :: Text
drawBoxFooter = "╰" <> Text.replicate (Term.width - 1) "─"

{----- Indentation -----}

type IndentLevel = Int

getIndentLevel :: TestInfo -> IndentLevel
getIndentLevel testInfo = length testInfo.contexts + 1

fullIndent :: IndentLevel -> Text -> Text
fullIndent = indentWith 4 " "

{----- Animation -----}

newtype AnimationThread = AnimationThread (MVar (Maybe (Async ())))

newAnimationThread :: IO AnimationThread
newAnimationThread = AnimationThread <$> newMVar Nothing

data Animation = Animation
  { fps :: Int
  , render :: Int -> Text
  }

mkAnimation :: [Text] -> Int -> Text
mkAnimation frames =
  -- Do not eta-expand this; this should cache the frames correctly
  (frames !!) . toIndex
 where
  toIndex t = t `mod` length frames

instance HasField "start" AnimationThread (Animation -> IO ()) where
  getField (AnimationThread mThreadVar) animation = modifyMVar_ mThreadVar $ \mThread -> do
    traverse_ Async.uninterruptibleCancel mThread
    thread <- Async.async (loop 0)
    pure $ Just thread
   where
    loop (i :: Int) = do
      Term.outputInPlace $ animation.render i
      threadDelay (1000000 `div` animation.fps)
      loop (i + 1)

instance HasField "clear" AnimationThread (IO ()) where
  getField (AnimationThread mThreadVar) = modifyMVar_ mThreadVar $ \mThread -> do
    traverse_ Async.uninterruptibleCancel mThread
    Term.clearInPlace
    pure Nothing
