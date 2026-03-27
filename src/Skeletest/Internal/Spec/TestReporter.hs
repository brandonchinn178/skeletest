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

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import GHC.Records (HasField (..))
import Skeletest.Internal.CLI (FormatFlag (..), getFormatFlag)
import Skeletest.Internal.Exit (TestExitCode)
import Skeletest.Internal.Spec.Output (BoxSpec, BoxSpecContent (..))
import Skeletest.Internal.TestInfo (TestInfo (..))
import Skeletest.Internal.TestRunner (TestResult (..), TestResultMessage (..))
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Internal.Utils.Text (indentWith)
import Skeletest.Internal.Utils.Timer (renderDuration)
import System.IO qualified as IO

{----- TestReporter -----}

data TestReporter = TestReporter
  { format :: FormatFlag
  , supportsANSI :: Bool
  }

newTestReporter :: IO TestReporter
newTestReporter = do
  format <- getFormatFlag
  supportsANSI <- Term.supportsANSI Term.stdout
  pure TestReporter{format, supportsANSI}

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
formatActionsMinimal = error "not implemented"

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

  reportTestPre _ testInfo = do
    Term.outputN $ fullIndent indentLevel (testInfo.name <> ": ")
    IO.hFlush IO.stdout
   where
    indentLevel = getIndentLevel testInfo

  reportTestPost reporter testInfo (result, duration) = do
    withBoxHeader $ do
      Term.output $ result.testResultLabel <> durationLabel
    case result.testResultMessage of
      TestResultMessageNone -> pure ()
      TestResultMessageInline msg -> do
        Term.output $ fullIndent (indentLevel + 1) msg
      TestResultMessageBox box -> do
        Term.outputN $ drawBoxBody box
        Term.output drawBoxFooter
   where
    indentLevel = getIndentLevel testInfo
    isBox = \case
      TestResultMessageBox _ -> True
      _ -> False
    withBoxHeader action
      | not $ isBox result.testResultMessage = do
          action
      | reporter.supportsANSI = do
          Term.outputN "\r"
          Term.outputN $ drawBoxHeader indentLevel (BoxHeaderType_Inline $ testInfo.name <> ": ")
          action
      | otherwise = do
          action
          Term.output $ drawBoxHeader indentLevel BoxHeaderType_NextLine
    durationLabel =
      if reporter.format == FormatFlag_Verbose || duration > 0.1
        then " " <> Color.gray ("(" <> renderDuration duration <> ")")
        else ""

-- Verbose is the same as full, except with some minor changes, so
-- we'll re-use full and inspect format directly
formatActionsVerbose :: FormatActions
formatActionsVerbose = formatActionsFull

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
