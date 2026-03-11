{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  X.Spec,
  X.SpecTree (..),

  -- ** Execution
  runSpecs,
  TestSummary (..),
  newTestSummary,

  -- ** Entrypoint
  X.SpecRegistry,
  X.SpecInfo (..),

  -- ** Defining a Spec
  X.describe,
  X.Testable (..),
  X.test,
  X.it,
  X.prop,

  -- ** Modifiers
  X.xfail,
  X.skip,
  X.focus,
  X.markManual,

  -- ** Markers
  X.IsMarker (..),
  X.withMarkers,
  X.withMarker,

  -- ** Built-in hooks
  applyTestSelectionsHook,
  manualTestsHook,
  xfailHook,
  skipHook,
  focusHook,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM)
import Control.Monad.Trans.State.Strict qualified as State
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Records (HasField (..))
import Numeric (showFFloat)
import Skeletest.Internal.Capture (addCapturedOutput, withCaptureOutput)
import Skeletest.Internal.Exit (TestExitCode (..))
import Skeletest.Internal.Fixtures (FixtureScopeKey (..), cleanupFixtures)
import Skeletest.Internal.Markers (
  findMarker,
 )
import Skeletest.Internal.Spec.Output (
  reportGroup,
  reportTestInProgress,
  reportTestResultWithBoxMessage,
  reportTestResultWithInlineMessage,
  reportTestResultWithoutMessage,
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
  testResultFromAssertionFail,
  testResultFromError,
 )
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Internal.Utils.Text (pluralize)
import Skeletest.Plugin (Hooks (..), defaultHooks, filterSpecTests, hasMarker)
import System.Console.Terminal.Size qualified as Term
import UnliftIO.Exception (
  finally,
  fromException,
  try,
 )

{----- Execute spec -----}

-- | Run the given Specs and return whether all of the tests passed.
runSpecs :: Hooks -> TestSummary -> SpecRegistry -> IO TestExitCode
runSpecs hooks testSummary specs = withTestSummary $ do
  (`finally` cleanupFixtures PerSessionFixtureKey) $
    fmap resolveExitCode . forM (pruneSpec specs) $ \SpecInfo{..} ->
      (`finally` cleanupFixtures (PerFileFixtureKey specPath)) $ do
        let emptyTestInfo =
              TestInfo
                { contexts = []
                , name = ""
                , markers = []
                , file = specPath
                }
        Text.putStrLn $ Text.pack specPath
        let specTrees = getSpecTrees specSpec
        runTrees emptyTestInfo specTrees
 where
  withTestSummary action = do
    testSummary.update $ \d -> d{testsSelected = getTotalTests specs}
    recordDuration testSummary action

  runTrees baseTestInfo = fmap resolveExitCode . mapM (runTree baseTestInfo)
  runTree baseTestInfo = \case
    SpecTree_Group{..} -> do
      let lvl = getIndentLevel baseTestInfo
      reportGroup lvl label
      runTrees baseTestInfo{TestInfo.contexts = baseTestInfo.contexts <> [label]} trees
    SpecTree_Test test -> do
      let lvl = getIndentLevel baseTestInfo
      reportTestInProgress lvl test.name

      let testInfo =
            baseTestInfo
              { TestInfo.name = test.name
              , TestInfo.markers = test.markers
              }
      TestResult{..} <-
        withTestInfo testInfo $ do
          tid <- myThreadId
          (result, duration) <-
            (`finally` cleanupFixtures (PerTestFixtureKey tid)) $ do
              withTimer $ runTest testInfo test.action
          let durationLabel =
                if duration < 0.1
                  then ""
                  else " " <> Color.gray ("(" <> renderDuration duration <> ")")
          pure result{testResultLabel = result.testResultLabel <> durationLabel}

      testSummary.update $ \d ->
        if
          | "SKIP" `Text.isInfixOf` testResultLabel -> d
          | testResultSuccess -> d{testsPassed = d.testsPassed + 1}
          | otherwise -> d{testsFailed = d.testsFailed + 1}

      case testResultMessage of
        TestResultMessageNone -> do
          reportTestResultWithoutMessage testResultLabel
        TestResultMessageInline msg -> do
          reportTestResultWithInlineMessage lvl testResultLabel msg
        TestResultMessageBox box -> do
          termSize <- Term.size
          reportTestResultWithBoxMessage termSize lvl test.name testResultLabel box
      pure $ if testResultSuccess then ExitSuccess else ExitTestFailure

  runTest info action =
    hooks.runTest info $ do
      (mCapture, resultOrError) <- withCaptureOutput (try action)
      case resultOrError of
        Right result -> pure result
        Left e ->
          fmap (addCapturedOutput mCapture) $
            case fromException e of
              Just e' -> testResultFromAssertionFail e'
              Nothing -> testResultFromError e

  getIndentLevel testInfo = length testInfo.contexts + 1 -- +1 to include the module name

-- | Resolve the given exit codes, returning the first non-success code.
resolveExitCode :: [TestExitCode] -> TestExitCode
resolveExitCode = go
 where
  go = \case
    [] -> ExitSuccess
    ExitSuccess : rest -> go rest
    code : _ -> code

withTimer :: IO a -> IO (a, NominalDiffTime)
withTimer m = do
  start <- getCurrentTime
  a <- m
  end <- getCurrentTime
  pure (a, end `diffUTCTime` start)

renderDuration :: NominalDiffTime -> Text
renderDuration duration = (Text.pack . showRounded) duration <> "s"
 where
  showRounded n = showFFloat (Just 2) (realToFrac n :: Double) ""

{----- Test summary -----}

newtype TestSummary = TestSummary (IORef TestSummaryData)

data TestSummaryData = TestSummaryData
  { totalTests :: !Int
  , testsSelected :: !Int
  , testsPassed :: !Int
  , testsFailed :: !Int
  , snapshotsUpdated :: !Int
  , totalDuration :: !NominalDiffTime
  }

newTestSummary :: SpecRegistry -> IO TestSummary
newTestSummary specs = do
  fmap TestSummary . newIORef $
    TestSummaryData
      { totalTests = getTotalTests specs
      , testsSelected = 0
      , testsPassed = 0
      , testsFailed = 0
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
    let testsSkipped = testsSelected - testsPassed - testsFailed
    pure . Text.unlines . concat $
      [ ["═════ Test report ═════"]
      , ["➤ " <> pluralize testsSelected "test" <> " ran in " <> renderDuration totalDuration]
      , when_ (testsFailed > 0) $
          "  • " <> pluralize testsFailed "test" <> " failed " <> Color.red "✘"
      , when_ (testsSkipped > 0) $
          "  • " <> pluralize testsSkipped "test" <> " skipped " <> Color.yellow "≫"
      , when_ (testsDeselected > 0) $
          "  • " <> pluralize testsDeselected "test" <> " deselected"
      ]
   where
    when_ p x = if p then [x] else []

{----- Built-in hooks -----}

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
  modify reason TestResult{..} =
    if testResultSuccess
      then
        TestResult
          { testResultSuccess = False
          , testResultLabel = Color.red "XPASS"
          , testResultMessage = TestResultMessageInline reason
          }
      else
        TestResult
          { testResultSuccess = True
          , testResultLabel = Color.yellow "XFAIL"
          , testResultMessage = TestResultMessageInline reason
          }

skipHook :: Hooks
skipHook =
  defaultHooks
    { runTest = \testInfo runTest ->
        case findMarker (testInfo.markers) of
          Just (MarkerSkip reason) ->
            pure
              TestResult
                { testResultSuccess = True
                , testResultLabel = Color.yellow "SKIP"
                , testResultMessage = TestResultMessageInline reason
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
  hasFocus = any (anySpecTests isFocused . (.specSpec))
  anySpecTests f spec =
    let go = \case
          SpecTree_Group{trees} -> concatMap go trees
          SpecTree_Test test -> [test]
     in any f $ concatMap go (getSpecTrees spec)
  isFocused test = hasMarker @MarkerFocus test.markers
  hideNotFocused = filterSpecTests isFocused
