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

  -- ** Markers
  IsMarker (..),
  withMarker,
  withMarkers,
  withManualMarkers,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM, guard)
import Control.Monad.Trans.Reader qualified as Trans
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Stack qualified as GHC
import System.Console.ANSI qualified as ANSI
import UnliftIO.Exception (
  Exception,
  SomeException,
  displayException,
  finally,
  fromException,
  throwIO,
  try,
  trySyncOrAsync,
 )

import Skeletest.Assertions (TestFailure (..))
import Skeletest.Internal.Fixtures (FixtureScopeKey (..), cleanupFixtures)
import Skeletest.Internal.Markers (
  AnonMarker (..),
  IsMarker (..),
  SomeMarker (..),
  findMarker,
 )
import Skeletest.Internal.State (TestInfo (TestInfo), withTestInfo)
import Skeletest.Internal.State qualified as TestInfo (TestInfo (..))
import Skeletest.Internal.TestTargets (TestTarget, TestTargets, matchesTest)
import Skeletest.Internal.TestTargets qualified as TestTargets
import Skeletest.Prop.Internal (Property, runProperty)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

withSpecTrees :: Monad m => ([SpecTree] -> m [SpecTree]) -> Spec -> m Spec
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
  Monad m =>
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
--
-- TODO: allow running tests in parallel
-- TODO: print summary: # total tests, # failing tests, # snapshots updated
runSpecs :: SpecRegistry -> IO Bool
runSpecs specs =
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
    runTrees testInfo = fmap and . mapM (runTree testInfo)
    runTree testInfo = \case
      SpecGroup{..} -> do
        let lvl = getIndentLevel testInfo
        Text.putStrLn $ indent lvl groupLabel
        runTrees testInfo{TestInfo.testContexts = TestInfo.testContexts testInfo <> [groupLabel]} groupTrees
      SpecTest{..} -> do
        let lvl = getIndentLevel testInfo
        Text.putStr $ indent lvl (testName <> ": ")

        -- TODO: timeout
        let testInfo' =
              testInfo
                { TestInfo.testName = testName
                , TestInfo.testMarkers = testMarkers
                }
        result <-
          trySyncOrAsync $
            withTestInfo testInfo' $ do
              tid <- myThreadId
              modifyTest testInfo' testAction `finally` cleanupFixtures (PerTestFixtureKey tid)

        case result of
          Right () -> do
            Text.putStrLn $ green "OK"
            pure True
          Left e
            | Just (SkeletestSkip reason) <- fromException e -> do
                Text.putStrLn $ yellow "SKIP"
                Text.putStrLn $ indent (lvl + 1) reason
                pure True
            | Just (SkeletestXFail reason) <- fromException e -> do
                Text.putStrLn $ yellow "XFAIL"
                Text.putStrLn $ indent (lvl + 1) reason
                pure True
            | Just (SkeletestXPass reason) <- fromException e -> do
                Text.putStrLn $ red "XPASS"
                Text.putStrLn $ indent (lvl + 1) reason
                pure False
            | Just failure <- fromException e -> do
                Text.putStrLn $ red "FAIL"
                Text.putStrLn =<< renderTestFailure failure
                pure False
            | otherwise -> do
                Text.putStrLn $ red "ERROR"
                Text.putStrLn $ indent (lvl + 1) (Text.pack $ displayException e)
                pure False

    -- TODO: make pluggable
    modifyTest info = foldr (.) id $ map ($ info) [checkXFail, checkSkip]

    getIndentLevel testInfo = length (TestInfo.testContexts testInfo) + 1 -- +1 to include the module name
    indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

    withANSI codes s = Text.pack (ANSI.setSGRCode codes) <> s <> Text.pack (ANSI.setSGRCode [ANSI.Reset])
    green = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    red = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
    yellow = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]

-- | Render a test failure like:
--
-- @
-- At test/Skeletest/Internal/TestTargetsSpec.hs:19:
-- |
-- |           parseTestTargets input `shouldBe` Right (Just expected)
-- |                                   ^^^^^^^^
--
-- Right 1 ≠ Left 1
-- @
renderTestFailure :: TestFailure -> IO Text
renderTestFailure TestFailure{..} = do
  prettyStackTrace <- mapM renderCallLine . reverse $ GHC.getCallStack callStack
  pure . Text.intercalate "\n" $
    [ border
    , Text.intercalate "\n\n" prettyStackTrace
    , ""
    , testFailMessage
    , border
    ]
  where
    border = Text.replicate 80 "-"

    renderCallLine (_, loc) = do
      let
        path = GHC.srcLocFile loc
        lineNum = GHC.srcLocStartLine loc
        startCol = GHC.srcLocStartCol loc
        endCol = GHC.srcLocEndCol loc

      mLine <-
        try (Text.readFile path) >>= \case
          Right srcFile -> pure $ getLineNum lineNum srcFile
          Left (_ :: SomeException) -> pure Nothing
      let (srcLine, pointerLine) =
            case mLine of
              Just line ->
                ( line
                , Text.replicate (startCol - 1) " " <> Text.replicate (endCol - startCol) "^"
                )
              Nothing ->
                ( "<unknown line>"
                , ""
                )

      pure . Text.intercalate "\n" $
        [ Text.pack path <> ":" <> (Text.pack . show) lineNum <> ":"
        , "|"
        , "| " <> srcLine
        , "| " <> pointerLine
        ]

    getLineNum n = listToMaybe . take 1 . drop (n - 1) . Text.lines

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

applyTestSelections :: TestTargets -> SpecRegistry -> SpecRegistry
applyTestSelections = \case
  Just selections -> map (applyTestSelections' selections)
  -- if no selections are specified, hide manual tests
  Nothing -> map (\info -> info{specSpec = hideManualTests $ specSpec info})
  where
    hideManualTests = mapSpecTrees (\go -> filter (not . isManualTest) . map go)
    isManualTest = \case
      SpecGroup{} -> False
      SpecTest{testMarkers} -> or [isManualMarker marker | SomeMarker marker <- testMarkers]

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
                , testMarkers = [getMarkerName m | SomeMarker m <- testMarkers]
                }
        pure $
          if matchesTest selections attrs
            then Just stest
            else Nothing

    mapMaybeM f = fmap catMaybes . mapM f

{----- Defining a Spec -----}

describe :: String -> Spec -> Spec
describe name = runIdentity . withSpecTrees (pure . (:[]) . mkGroup)
  where
    mkGroup trees =
      SpecGroup
        { groupLabel = Text.pack name
        , groupTrees = trees
        }

class Testable a where
  runTest :: a -> IO ()

instance Testable (IO ()) where
  runTest = id

instance Testable Property where
  runTest = runProperty

test :: Testable a => String -> a -> Spec
test name t = Spec $ tell [mkTest]
  where
    mkTest =
      SpecTest
        { testName = Text.pack name
        , testMarkers = []
        , testAction = runTest t
        }

it :: String -> IO () -> Spec
it = test

prop :: String -> Property -> Spec
prop = test

{----- Modifiers -----}

-- | Mark the given spec as expected to fail.
-- Fails tests if they unexpectedly pass.
--
-- Can be selected with the marker '@xfail'
xfail :: String -> Spec -> Spec
xfail = withMarker . MarkerXFail . Text.pack

checkXFail :: TestInfo -> IO a -> IO a
checkXFail info m =
  case findMarker (TestInfo.testMarkers info) of
    Just (MarkerXFail reason) ->
      try m >>= \case
        Left (_ :: SomeException) -> throwIO $ SkeletestXFail reason
        Right _ -> throwIO $ SkeletestXPass reason
    Nothing -> m

-- TODO: instead of throwing xfail, allow checkXFail to
-- explicitly modify the test result
data SkeletestXFail = SkeletestXFail Text
  deriving (Show)

instance Exception SkeletestXFail

data SkeletestXPass = SkeletestXPass Text
  deriving (Show)

instance Exception SkeletestXPass

-- | Skip all tests in the given spec.
--
-- Can be selected with the marker '@skip'
skip :: String -> Spec -> Spec
skip = withMarker . MarkerSkip . Text.pack

checkSkip :: TestInfo -> IO a -> IO a
checkSkip info m =
  case findMarker (TestInfo.testMarkers info) of
    Just (MarkerSkip reason) -> throwIO $ SkeletestSkip reason
    Nothing -> m

-- TODO: instead of throwing skip, allow checkSkip to
-- explicitly modify the test result
data SkeletestSkip = SkeletestSkip Text
  deriving (Show)

instance Exception SkeletestSkip

{----- Markers -----}

newtype MarkerXFail = MarkerXFail Text
  deriving (Show)

instance IsMarker MarkerXFail where
  getMarkerName _ = "xfail"

newtype MarkerSkip = MarkerSkip Text
  deriving (Show)

instance IsMarker MarkerSkip where
  getMarkerName _ = "skip"

-- | Adds the given marker to all the tests in the given spec.
--
-- Useful for selecting tests from the command line or identifying tests in hooks
withMarker :: IsMarker a => a -> Spec -> Spec
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
withMarkers = withMarkers' False

-- | Adds the given names as manual markers to all tests in the given spec.
--
-- See 'isManualMarker'.
withManualMarkers :: [String] -> Spec -> Spec
withManualMarkers = withMarkers' True

withMarkers' :: Bool -> [String] -> Spec -> Spec
withMarkers' isManual = foldr (\mark acc -> withMarker (toTag mark) . acc) id
  where
    toTag name =
      AnonMarker
        { anonMarkerName = Text.pack name
        , anonMarkerManual = isManual
        }
