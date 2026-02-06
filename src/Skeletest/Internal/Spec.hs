{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  X.Spec,
  X.SpecTree (..),

  -- ** Execution
  runSpecs,

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
  X.markManual,

  -- ** Markers
  X.IsMarker (..),
  X.withMarkers,
  X.withMarker,

  -- ** Built-in hooks
  xfailHook,
  skipHook,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Skeletest.Internal.Capture (addCapturedOutput, withCaptureOutput)
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
  MarkerSkip (..),
  MarkerXFail (..),
  SpecInfo (..),
  SpecRegistry,
  SpecTree (..),
  getSpecTrees,
  pruneSpec,
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
import Skeletest.Plugin (Hooks (..), defaultHooks)
import System.Console.Terminal.Size qualified as Term
import UnliftIO.Exception (
  finally,
  fromException,
  try,
 )

{----- Execute spec -----}

-- | Run the given Specs and return whether all of the tests passed.
runSpecs :: Hooks -> SpecRegistry -> IO Bool
runSpecs Hooks{..} specs =
  (`finally` cleanupFixtures PerSessionFixtureKey) $
    fmap and . forM (pruneSpec specs) $ \SpecInfo{..} ->
      (`finally` cleanupFixtures (PerFileFixtureKey specPath)) $ do
        let emptyTestInfo =
              TestInfo
                { testContexts = []
                , testName = ""
                , testMarkers = []
                , testFile = specPath
                }
        Text.putStrLn $ Text.pack specPath
        specTrees <- hookModifyFileSpecs $ getSpecTrees specSpec
        runTrees emptyTestInfo specTrees
 where
  runTrees baseTestInfo = fmap and . mapM (runTree baseTestInfo)
  runTree baseTestInfo = \case
    SpecGroup{..} -> do
      let lvl = getIndentLevel baseTestInfo
      reportGroup lvl groupLabel
      runTrees baseTestInfo{TestInfo.testContexts = TestInfo.testContexts baseTestInfo <> [groupLabel]} groupTrees
    SpecTest{..} -> do
      let lvl = getIndentLevel baseTestInfo
      reportTestInProgress lvl testName

      let testInfo =
            baseTestInfo
              { TestInfo.testName = testName
              , TestInfo.testMarkers = testMarkers
              }
      TestResult{..} <-
        withTestInfo testInfo $ do
          tid <- myThreadId
          runTest testInfo testAction `finally` cleanupFixtures (PerTestFixtureKey tid)

      case testResultMessage of
        TestResultMessageNone -> do
          reportTestResultWithoutMessage testResultLabel
        TestResultMessageInline msg -> do
          reportTestResultWithInlineMessage lvl testResultLabel msg
        TestResultMessageBox box -> do
          termSize <- Term.size
          reportTestResultWithBoxMessage termSize lvl testName testResultLabel box
      pure testResultSuccess

  runTest info action =
    hookRunTest info $ do
      (mCapture, resultOrError) <- withCaptureOutput (try action)
      case resultOrError of
        Right result -> pure result
        Left e ->
          fmap (addCapturedOutput mCapture) $
            case fromException e of
              Just e' -> testResultFromAssertionFail e'
              Nothing -> testResultFromError e

  getIndentLevel testInfo = length (TestInfo.testContexts testInfo) + 1 -- +1 to include the module name

{----- Built-in hooks -----}

xfailHook :: Hooks
xfailHook =
  defaultHooks
    { hookRunTest = \testInfo runTest ->
        case findMarker (TestInfo.testMarkers testInfo) of
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
    { hookRunTest = \testInfo runTest ->
        case findMarker (TestInfo.testMarkers testInfo) of
          Just (MarkerSkip reason) ->
            pure
              TestResult
                { testResultSuccess = True
                , testResultLabel = Color.yellow "SKIP"
                , testResultMessage = TestResultMessageInline reason
                }
          Nothing -> runTest
    }
