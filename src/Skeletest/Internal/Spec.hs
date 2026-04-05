{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  X.Spec,
  X.SpecTree (..),

  -- ** Execution
  SpecRunner,
  newSpecRunner,

  -- ** Entrypoint
  X.SpecRegistry,
  X.SpecInfo (..),

  -- ** Defining a Spec
  X.describe,
  X.Testable (..),
  X.test,
  X.it,

  -- ** Modifiers
  X.xfail,
  X.skip,
  X.focus,
  X.markManual,

  -- ** Markers
  X.IsMarker (..),
  X.withMarkers,
  X.withMarker,

  -- ** Plugin
  specTreePlugin,
) where

import Control.Concurrent (myThreadId)
import Control.Monad.Trans.State.Strict qualified as State
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import GHC.Records (HasField (..))
import Skeletest.Internal.Exit (TestExitCode (..))
import Skeletest.Internal.Fixtures (FixtureScopeKey (..), cleanupFixtures)
import Skeletest.Internal.Markers (
  findMarker,
 )
import Skeletest.Internal.Spec.TestReporter (
  TestReporter,
  newTestReporter,
 )
import Skeletest.Internal.Spec.Tree (
  MarkerFocus (..),
  MarkerManual (..),
  MarkerSkip (..),
  MarkerXFail (..),
  SpecInfo (..),
  SpecRegistry,
  SpecTest (..),
  SpecTree (..),
  applyTestSelections,
  getSpecTrees,
  mapSpecs,
  pruneSpec,
  traverseSpecTests,
  traverseSpecs,
 )
import Skeletest.Internal.Spec.Tree qualified as X
import Skeletest.Internal.TestInfo (TestInfo (TestInfo), withTestInfo)
import Skeletest.Internal.TestInfo qualified as TestInfo
import Skeletest.Internal.TestRunner (
  TestResult (..),
  TestResultMessage (..),
  TestResultStatus (..),
  testResultFromAssertionFail,
  testResultFromError,
 )
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Term qualified as Term
import Skeletest.Internal.Utils.Text (pluralize)
import Skeletest.Internal.Utils.Timer (renderDuration, withTimer)
import Skeletest.Plugin (Hooks (..), Plugin (..), defaultHooks, defaultPlugin, filterSpecTests, hasMarker)
import Skeletest.Plugin qualified as Plugin
import UnliftIO.Exception (
  catch,
  finally,
  fromException,
 )

{----- Execute spec -----}

data SpecRunner = SpecRunner
  { hooks :: Hooks
  , testSummary :: TestSummary
  , testReporter :: TestReporter
  }

newSpecRunner :: Hooks -> SpecRegistry -> IO SpecRunner
newSpecRunner hooks initialSpecs = do
  testSummary <- newTestSummary initialSpecs
  testReporter <- newTestReporter
  pure
    SpecRunner
      { hooks
      , testSummary
      , testReporter
      }

instance HasField "run" SpecRunner (SpecRegistry -> IO TestExitCode) where
  getField runner specs = withTestSummary $ do
    (`finally` cleanupFixtures PerSessionFixtureKey) $
      resolveExitCode <$> mapM runner.runFile (pruneSpec specs)
   where
    withTestSummary action = do
      runner.testSummary.update $ \d -> d{testsSelected = getTotalTests specs}
      recordDuration runner.testSummary action

instance HasField "runFile" SpecRunner (SpecInfo -> IO TestExitCode) where
  getField runner info = do
    (`finally` cleanupFixtures (PerFileFixtureKey info.specPath)) $ do
      runner.testReporter.reportFilePre info.specPath
      (code, duration) <- withTimer $ runner.runTrees emptyTestInfo trees
      runner.testReporter.reportFilePost info.specPath (code, duration)
      pure code
   where
    trees = getSpecTrees info.spec
    emptyTestInfo =
      TestInfo
        { contexts = []
        , name = ""
        , markers = []
        , file = info.specPath
        }

instance HasField "runTrees" SpecRunner (TestInfo -> [SpecTree] -> IO TestExitCode) where
  getField runner testInfo = fmap resolveExitCode . mapM runTree
   where
    runTree = \case
      SpecTree_Group{label, trees} -> runner.runGroup testInfo label trees
      SpecTree_Test test ->
        runner.runTest
          testInfo
            { TestInfo.name = test.name
            , TestInfo.markers = test.markers
            }
          test

instance HasField "runGroup" SpecRunner (TestInfo -> Text -> [SpecTree] -> IO TestExitCode) where
  getField runner testInfo label trees = do
    runner.testReporter.reportGroupPre testInfo label
    (code, duration) <- withTimer $ runner.runTrees testInfo' trees
    runner.testReporter.reportGroupPost testInfo label (code, duration)
    pure code
   where
    testInfo' = testInfo{TestInfo.contexts = testInfo.contexts <> [label]}

instance HasField "runTest" SpecRunner (TestInfo -> SpecTest -> IO TestExitCode) where
  getField runner testInfo test = withTestInfo testInfo $ do
    runner.testReporter.reportTestPre testInfo
    tid <- myThreadId
    (result, duration) <-
      (`finally` cleanupFixtures (PerTestFixtureKey tid)) $ do
        withTimer . runner.hooks.runTest testInfo $ do
          test.action `catch` mkTestResultError
    runner.testReporter.reportTestPost testInfo (result, duration)

    runner.testSummary.update $ \d ->
      d
        { testCategories =
            Map.alter
              (Just . (+ 1) . fromMaybe 0)
              result.status
              d.testCategories
        }

    pure $ if result.status.success then ExitSuccess else ExitTestFailure
   where
    mkTestResultError e =
      case fromException e of
        Just e' -> testResultFromAssertionFail e'
        Nothing -> testResultFromError e

instance HasField "printSummary" SpecRunner (IO ()) where
  getField runner = do
    summary <- runner.testSummary.render >>= runner.hooks.modifyTestSummary
    Term.output ""
    Term.outputN . colorize . Text.strip $ summary
   where
    colorize = Text.unlines . map Color.yellow . Text.lines

-- | Resolve the given exit codes, returning the first non-success code.
resolveExitCode :: [TestExitCode] -> TestExitCode
resolveExitCode = go
 where
  go = \case
    [] -> ExitSuccess
    ExitSuccess : rest -> go rest
    code : _ -> code

{----- Test summary -----}

newtype TestSummary = TestSummary (IORef TestSummaryData)

data TestSummaryData = TestSummaryData
  { totalTests :: !Int
  , testsSelected :: !Int
  , testCategories :: !(Map TestResultStatus Int)
  , snapshotsUpdated :: !Int
  , totalDuration :: !NominalDiffTime
  }

newTestSummary :: SpecRegistry -> IO TestSummary
newTestSummary specs = do
  fmap TestSummary . newIORef $
    TestSummaryData
      { totalTests = getTotalTests specs
      , testsSelected = 0
      , testCategories = Map.empty
      , snapshotsUpdated = 0
      , totalDuration = 0
      }

getTotalTests :: SpecRegistry -> Int
getTotalTests specs = State.execState (count specs) 0
 where
  count = traverseSpecs . traverseSpecTests $ \x -> State.modify (+ 1) *> pure x

recordDuration :: TestSummary -> IO a -> IO a
recordDuration (TestSummary ref) m = do
  (a, duration) <- withTimer m
  modifyIORef' ref $ \d -> d{totalDuration = duration}
  pure a

instance HasField "update" TestSummary ((TestSummaryData -> TestSummaryData) -> IO ()) where
  getField (TestSummary ref) = modifyIORef' ref
instance HasField "render" TestSummary (IO Text) where
  getField (TestSummary ref) = do
    TestSummaryData{..} <- readIORef ref
    let testsDeselected = totalTests - testsSelected
    pure . Text.unlines . concat $
      [ ["═════ Test report ═════"]
      , ["➤ " <> pluralize testsSelected "test" <> " ran in " <> renderDuration totalDuration]
      , [ "  • " <> pluralize count "test" <> " " <> status.name <> " " <> icon
        | (status, count) <- Map.toAscList testCategories
        , let icon =
                case status of
                  TestPassed -> Color.green "✔"
                  TestFailed -> Color.red "✘"
                  TestSkipped -> Color.yellow "≫"
                  TestStatus{success_}
                    | success_ -> Color.green "✔"
                    | otherwise -> Color.red "✘"
        ]
      , when_ (testsDeselected > 0) $
          "  • " <> pluralize testsDeselected "test" <> " deselected"
      ]
   where
    when_ p x = if p then [x] else []

{----- Built-in hooks -----}

specTreePlugin :: Plugin
specTreePlugin =
  defaultPlugin
    { Plugin.hooks =
        mconcat
          [ xfailHook
          , skipHook
          , focusHook
          , applyTestSelectionsHook
          , manualTestsHook
          ]
    }

applyTestSelectionsHook :: Hooks
applyTestSelectionsHook =
  defaultHooks
    { modifySpecRegistry = \case
        Just selections -> \modify -> fmap (map (applyTestSelections selections)) . modify
        Nothing -> id
    }

manualTestsHook :: Hooks
manualTestsHook =
  defaultHooks
    { modifySpecRegistry = \case
        -- only hide manual tests when no selections are specified
        Just _ -> id
        Nothing -> \modify -> fmap (mapSpecs hideManual) . modify
    }
 where
  hideManual = filterSpecTests (not . hasMarker @MarkerManual . (.markers))

xfailHook :: Hooks
xfailHook =
  defaultHooks
    { runTest = \testInfo runTest ->
        case findMarker testInfo.markers of
          Just (MarkerXFail reason) -> modify reason <$> runTest
          Nothing -> runTest
    }
 where
  modify reason result =
    if result.status.success
      then
        TestResult
          { status =
              TestStatus
                { name_ = "xpassed"
                , success_ = False
                }
          , label = Color.red "XPASS"
          , message = TestResultMessageInline reason
          }
      else
        TestResult
          { status = TestPassed
          , label = Color.yellow "XFAIL"
          , message = TestResultMessageInline reason
          }

skipHook :: Hooks
skipHook =
  defaultHooks
    { runTest = \testInfo runTest ->
        case findMarker (testInfo.markers) of
          Just (MarkerSkip reason) ->
            pure
              TestResult
                { status = TestSkipped
                , label = Color.yellow "SKIP"
                , message = TestResultMessageInline reason
                }
          Nothing -> runTest
    }

focusHook :: Hooks
focusHook =
  defaultHooks
    { modifySpecRegistry = \_ modify -> fmap applyFocus . modify
    }
 where
  applyFocus specs = if hasFocus specs then mapSpecs hideNotFocused specs else specs
  hasFocus = any (anySpecTests isFocused . (.spec))
  anySpecTests f spec =
    let go = \case
          SpecTree_Group{trees} -> concatMap go trees
          SpecTree_Test test -> [test]
     in any f $ concatMap go (getSpecTrees spec)
  isFocused test = hasMarker @MarkerFocus test.markers
  hideNotFocused = filterSpecTests isFocused
