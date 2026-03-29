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
  testReporterPlugin,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import Skeletest.Internal.CLI qualified as CLI
import Skeletest.Internal.Exit (TestExitCode)
import Skeletest.Internal.Spec.Output (BoxSpec, BoxSpecContent (..))
import Skeletest.Internal.TestInfo (TestInfo (..))
import Skeletest.Internal.TestRunner (TestResult (..), TestResultMessage (..))
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Internal.Utils.Text (indentWith)
import Skeletest.Plugin (Plugin (..), defaultPlugin)
import System.IO qualified as IO

{----- TestReporter -----}

data TestReporter = TestReporter
  { format :: ReportFormat
  }

newTestReporter :: IO TestReporter
newTestReporter = do
  format <- fromMaybe ReportFormat_Full <$> CLI.getFlag -- TODO: change default to minimal
  pure TestReporter{format}

testReporterPlugin :: Plugin
testReporterPlugin =
  defaultPlugin
    { cliFlags = [CLI.flag @(Maybe ReportFormat)]
    }

getFormatAction :: forall field a. (HasField field FormatActions (TestReporter -> a)) => TestReporter -> a
getFormatAction reporter = getField @field formatActions reporter
 where
  formatActions =
    case reporter.format of
      ReportFormat_Minimal -> formatActionsMinimal
      ReportFormat_Full -> formatActionsFull
      ReportFormat_Verbose -> formatActionsVerbose

instance HasField "reportFilePre" TestReporter (FilePath -> IO ()) where
  getField = getFormatAction @"reportFilePre"
instance HasField "reportFilePost" TestReporter (FilePath -> TestExitCode -> IO ()) where
  getField = getFormatAction @"reportFilePost"
instance HasField "reportGroupPre" TestReporter (TestInfo -> Text -> IO ()) where
  getField = getFormatAction @"reportGroupPre"
instance HasField "reportGroupPost" TestReporter (TestInfo -> Text -> TestExitCode -> IO ()) where
  getField = getFormatAction @"reportGroupPost"
instance HasField "reportTestPre" TestReporter (TestInfo -> IO ()) where
  getField = getFormatAction @"reportTestPre"
instance HasField "reportTestPost" TestReporter (TestInfo -> TestResult -> IO ()) where
  getField = getFormatAction @"reportTestPost"

{----- Report formats -----}

data ReportFormat
  = ReportFormat_Minimal
  | ReportFormat_Full
  | ReportFormat_Verbose
  deriving (Show, Eq)

instance CLI.IsFlag (Maybe ReportFormat) where
  flagName = "format"
  flagHelp = "The format of the output"
  flagSpec =
    CLI.OptionalFlag
      { flagDefault = Nothing
      , flagParse = \case
          "minimal" -> Right $ Just ReportFormat_Minimal
          "full" -> Right $ Just ReportFormat_Full
          "verbose" -> Right $ Just ReportFormat_Verbose
          s -> Left $ "Unknown format: " <> s
      }

data FormatActions = FormatActions
  { reportFilePre :: TestReporter -> FilePath -> IO ()
  , reportFilePost :: TestReporter -> FilePath -> TestExitCode -> IO ()
  , reportGroupPre :: TestReporter -> TestInfo -> Text -> IO ()
  , reportGroupPost :: TestReporter -> TestInfo -> Text -> TestExitCode -> IO ()
  , reportTestPre :: TestReporter -> TestInfo -> IO ()
  , reportTestPost :: TestReporter -> TestInfo -> TestResult -> IO ()
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

  reportTestPost _ testInfo result = do
    case result.testResultMessage of
      TestResultMessageBox _ -> do
        Term.outputN "\r"
        Term.outputN $ drawBoxHeader indentLevel (testInfo.name <> ": ")
      _ -> pure ()
    Term.output result.testResultLabel
    case result.testResultMessage of
      TestResultMessageNone -> pure ()
      TestResultMessageInline msg -> do
        Term.output $ fullIndent (indentLevel + 1) msg
      TestResultMessageBox box -> do
        Term.outputN $ drawBoxBody box
        Term.output drawBoxFooter
   where
    indentLevel = getIndentLevel testInfo

formatActionsVerbose :: FormatActions
formatActionsVerbose = error "not implemented"

{----- BoxSpec -----}

drawBoxHeader :: IndentLevel -> Text -> Text
drawBoxHeader lvl s = "╭" <> dashes <> " " <> s
 where
  dashes = Text.replicate (4 * lvl - 2) "─"

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
