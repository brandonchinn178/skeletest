{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  applyTestSelectionsHook,
  manualTestsHook,
  xfailHook,
  skipHook,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM)
import Data.Maybe (isJust)
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
  MarkerManual (..),
  MarkerSkip (..),
  MarkerXFail (..),
  SpecInfo (..),
  SpecRegistry,
  SpecTree (..),
  applyTestSelections,
  getSpecTrees,
  mapSpecTrees,
  mapSpecs,
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
runSpecs hooks specs =
  (`finally` cleanupFixtures PerSessionFixtureKey) $
    fmap and . forM (pruneSpec specs) $ \SpecInfo{..} ->
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
  runTrees baseTestInfo = fmap and . mapM (runTree baseTestInfo)
  runTree baseTestInfo = \case
    SpecTree_Group{..} -> do
      let lvl = getIndentLevel baseTestInfo
      reportGroup lvl label
      runTrees baseTestInfo{TestInfo.contexts = baseTestInfo.contexts <> [label]} trees
    SpecTree_Test{..} -> do
      let lvl = getIndentLevel baseTestInfo
      reportTestInProgress lvl name

      let testInfo =
            baseTestInfo
              { TestInfo.name = name
              , TestInfo.markers = markers
              }
      TestResult{..} <-
        withTestInfo testInfo $ do
          tid <- myThreadId
          runTest testInfo action `finally` cleanupFixtures (PerTestFixtureKey tid)

      case testResultMessage of
        TestResultMessageNone -> do
          reportTestResultWithoutMessage testResultLabel
        TestResultMessageInline msg -> do
          reportTestResultWithInlineMessage lvl testResultLabel msg
        TestResultMessageBox box -> do
          termSize <- Term.size
          reportTestResultWithBoxMessage termSize lvl name testResultLabel box
      pure testResultSuccess

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
  hideManual = mapSpecTrees (\go -> filter (not . isManualTest) . map go)
  isManualTest = \case
    SpecTree_Group{} -> False
    SpecTree_Test{markers} -> isJust $ findMarker @MarkerManual markers

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
