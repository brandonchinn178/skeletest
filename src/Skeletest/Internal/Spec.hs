{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  Spec,
  SpecTree (..),
  runSpecs,

  -- ** Entrypoint
  SpecRegistry,
  SpecInfo (..),
  pruneSpec,
  applyTestSelections,

  -- ** Defining a Spec
  describe,
  Testable (..),
  test,
  it,
  prop,

  -- ** Modifiers
  xfail,
  skip,
  markManual,

  -- ** Markers
  IsMarker (..),
  withMarkers,
  withMarker,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM, guard)
import Control.Monad.Trans.Reader qualified as Trans
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import UnliftIO.Exception (
  finally,
  fromException,
  try,
 )

import Skeletest.Assertions (Testable, runTestable)
import Skeletest.Internal.Fixtures (FixtureScopeKey (..), cleanupFixtures)
import Skeletest.Internal.Markers (
  AnonMarker (..),
  IsMarker (..),
  SomeMarker (..),
  findMarker,
 )
import Skeletest.Internal.TestInfo (TestInfo (TestInfo), withTestInfo)
import Skeletest.Internal.TestInfo qualified as TestInfo
import Skeletest.Internal.TestRunner (
  TestResult (..),
  TestResultMessage (..),
  testResultFromAssertionFail,
  testResultFromError,
  testResultPass,
 )
import Skeletest.Internal.TestTargets (TestTarget, TestTargets, matchesTest)
import Skeletest.Internal.TestTargets qualified as TestTargets
import Skeletest.Internal.Utils.Color qualified as Color
import Skeletest.Plugin (Hooks (..), defaultHooks)
import Skeletest.Prop.Internal (Property)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

withSpecTrees :: (Monad m) => ([SpecTree] -> m [SpecTree]) -> Spec -> m Spec
withSpecTrees f = fmap (Spec . tell) . f . getSpecTrees

data SpecTree
  = SpecGroup
      { groupLabel :: Text
      , groupTrees :: [SpecTree]
      }
  | SpecTest
      { testName :: Text
      , testMarkers :: [SomeMarker]
      -- ^ Markers, in order from least to most recently applied.
      --
      -- >>> withMarker MarkerA . withMarker MarkerB $ test ...
      --
      -- will contain
      --
      -- >>> SpecTest { testMarkers = [MarkerA, MarkerB] }
      , testAction :: IO ()
      }

-- | Traverse the tree with the given processing function.
--
-- To preprocess trees with @pre@ and postprocess with @post@:
--
-- >>> traverseSpecTrees (\go -> post <=< mapM go <=< pre) spec
traverseSpecTrees ::
  forall m.
  (Monad m) =>
  ( (SpecTree -> m SpecTree)
    -> [SpecTree]
    -> m [SpecTree]
  )
  -> Spec
  -> m Spec
traverseSpecTrees f = withSpecTrees go
  where
    go :: [SpecTree] -> m [SpecTree]
    go = f recurseGroups

    recurseGroups = \case
      group@SpecGroup{} -> do
        trees' <- go $ groupTrees group
        pure group{groupTrees = trees'}
      stest@SpecTest{} -> pure stest

-- | Map the tree with the given processing function.
--
-- To preprocess trees with @pre@ and postprocess with @post@:
--
-- >>> mapSpecTrees (\go -> post . map go . pre) spec
mapSpecTrees ::
  ( (SpecTree -> SpecTree)
    -> [SpecTree]
    -> [SpecTree]
  )
  -> Spec
  -> Spec
mapSpecTrees f = runIdentity . traverseSpecTrees (\go -> pure . f (runIdentity . go))

{----- Execute spec -----}

-- | Run the given Specs and return whether all of the tests passed.
runSpecs :: Hooks -> SpecRegistry -> IO Bool
runSpecs hooks0 specs =
  (`finally` cleanupFixtures PerSessionFixtureKey) $
    fmap and . forM specs $ \SpecInfo{..} ->
      (`finally` cleanupFixtures (PerFileFixtureKey specPath)) $ do
        let emptyTestInfo =
              TestInfo
                { testModule = specName
                , testContexts = []
                , testName = ""
                , testMarkers = []
                , testFile = specPath
                }
        Text.putStrLn specName
        runTrees emptyTestInfo $ getSpecTrees specSpec
  where
    Hooks{..} = builtinHooks <> hooks0
    builtinHooks = xfailHook <> skipHook

    runTrees baseTestInfo = fmap and . mapM (runTree baseTestInfo)
    runTree baseTestInfo = \case
      SpecGroup{..} -> do
        let lvl = getIndentLevel baseTestInfo
        Text.putStrLn $ indent lvl groupLabel
        runTrees baseTestInfo{TestInfo.testContexts = TestInfo.testContexts baseTestInfo <> [groupLabel]} groupTrees
      SpecTest{..} -> do
        let lvl = getIndentLevel baseTestInfo
        Text.putStr $ indent lvl (testName <> ": ")

        let testInfo =
              baseTestInfo
                { TestInfo.testName = testName
                , TestInfo.testMarkers = testMarkers
                }
        TestResult{..} <-
          withTestInfo testInfo $ do
            tid <- myThreadId
            runTest testInfo testAction `finally` cleanupFixtures (PerTestFixtureKey tid)

        Text.putStrLn testResultLabel
        case testResultMessage of
          TestResultMessageNone -> pure ()
          TestResultMessageInline msg -> Text.putStrLn $ indent (lvl + 1) msg
          TestResultMessageSection msg -> Text.putStrLn $ withBorder msg
        pure testResultSuccess

    runTest info action =
      hookRunTest info $ do
        try action >>= \case
          Right () -> pure testResultPass
          Left e
            | Just e' <- fromException e -> testResultFromAssertionFail e'
            | otherwise -> pure $ testResultFromError e

    getIndentLevel testInfo = length (TestInfo.testContexts testInfo) + 1 -- +1 to include the module name
    indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

    border = Text.replicate 80 "-"
    withBorder msg = Text.intercalate "\n" [border, msg, border]

{----- Entrypoint -----}

type SpecRegistry = [SpecInfo]

data SpecInfo = SpecInfo
  { specPath :: FilePath
  , specName :: Text
  , specSpec :: Spec
  }

pruneSpec :: SpecRegistry -> SpecRegistry
pruneSpec = mapMaybe $ \info -> do
  let spec = mapSpecTrees (\go -> filter (not . isEmptySpec) . map go) (specSpec info)
  guard $ (not . null . getSpecTrees) spec
  pure info{specSpec = spec}
  where
    isEmptySpec = \case
      SpecGroup _ [] -> True
      _ -> False

-- TODO: make hookable? implement manual tests with hook?
applyTestSelections :: TestTargets -> SpecRegistry -> SpecRegistry
applyTestSelections = \case
  Just selections -> map (applyTestSelections' selections)
  -- if no selections are specified, hide manual tests
  Nothing -> map (\info -> info{specSpec = hideManualTests $ specSpec info})
  where
    hideManualTests = mapSpecTrees (\go -> filter (not . isManualTest) . map go)
    isManualTest = \case
      SpecGroup{} -> False
      SpecTest{testMarkers} -> isJust $ findMarker @MarkerManual testMarkers

applyTestSelections' :: TestTarget -> SpecInfo -> SpecInfo
applyTestSelections' selections info = info{specSpec = applySelections $ specSpec info}
  where
    applySelections = (`Trans.runReader` []) . traverseSpecTrees apply

    apply go = mapMaybeM $ \case
      group@SpecGroup{groupLabel} -> Just <$> Trans.local (<> [groupLabel]) (go group)
      stest@SpecTest{testName, testMarkers} -> do
        groups <- Trans.ask
        let attrs =
              TestTargets.TestAttrs
                { testPath = specPath info
                , testIdentifier = groups <> [testName]
                , testMarkers = [Text.pack $ getMarkerName m | SomeMarker m <- testMarkers]
                }
        pure $
          if matchesTest selections attrs
            then Just stest
            else Nothing

    mapMaybeM f = fmap catMaybes . mapM f

{----- Defining a Spec -----}

describe :: String -> Spec -> Spec
describe name = runIdentity . withSpecTrees (pure . (: []) . mkGroup)
  where
    mkGroup trees =
      SpecGroup
        { groupLabel = Text.pack name
        , groupTrees = trees
        }

test :: (Testable m) => String -> m () -> Spec
test name t = Spec $ tell [mkTest]
  where
    mkTest =
      SpecTest
        { testName = Text.pack name
        , testMarkers = []
        , testAction = runTestable t
        }

it :: String -> IO () -> Spec
it = test

prop :: String -> Property -> Spec
prop = test

{----- Modifiers -----}

-- | Mark the given spec as expected to fail.
-- Fails tests if they unexpectedly pass.
--
-- Can be selected with the marker @@xfail@
xfail :: String -> Spec -> Spec
xfail = withMarker . MarkerXFail . Text.pack

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

-- | Skip all tests in the given spec.
--
-- Can be selected with the marker @@skip@
skip :: String -> Spec -> Spec
skip = withMarker . MarkerSkip . Text.pack

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

markManual :: Spec -> Spec
markManual = withMarker MarkerManual

{----- Markers -----}

newtype MarkerXFail = MarkerXFail Text
  deriving (Show)

instance IsMarker MarkerXFail where
  getMarkerName _ = "xfail"

newtype MarkerSkip = MarkerSkip Text
  deriving (Show)

instance IsMarker MarkerSkip where
  getMarkerName _ = "skip"

data MarkerManual = MarkerManual
  deriving (Show)

instance IsMarker MarkerManual where
  getMarkerName _ = "manual"

-- | Adds the given marker to all the tests in the given spec.
--
-- Useful for selecting tests from the command line or identifying tests in hooks
withMarker :: (IsMarker a) => a -> Spec -> Spec
withMarker m = mapSpecTrees (\go -> map (addMarker . go))
  where
    marker = SomeMarker m
    addMarker = \case
      group@SpecGroup{} -> group
      tree@SpecTest{} -> tree{testMarkers = marker : testMarkers tree}

-- | Adds the given names as plain markers to all tests in the given spec.
--
-- See 'getMarkerName'.
withMarkers :: [String] -> Spec -> Spec
withMarkers = foldr (\name acc -> withMarker (AnonMarker name) . acc) id
