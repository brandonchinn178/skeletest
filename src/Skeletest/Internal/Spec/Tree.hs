{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Spec.Tree (
  -- * Spec interface
  Spec,
  SpecTree (..),
  SpecTest (..),

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
  MarkerXFail (..),
  xfail,
  MarkerSkip (..),
  skip,
  MarkerFocus (..),
  focus,
  MarkerManual (..),
  markManual,

  -- ** Markers
  IsMarker (..),
  withMarkers,
  withMarker,

  -- ** Internal API
  getSpecTrees,
  withSpecTrees,
  mapSpecTrees,
  traverseSpecTrees,
  getSpecTests,
  mapSpecTests,
  filterSpecTests,
  traverseSpecTests,
  mapSpecs,
  traverseSpecs,
) where

import Control.Monad (guard, (>=>))
import Control.Monad.Trans.Reader qualified as Trans
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Foldable qualified as Seq
import Data.Functor.Identity (runIdentity)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest.Assertions (Testable, runTestable)
import Skeletest.Internal.Markers (
  AnonMarker (..),
  IsMarker (..),
  SomeMarker (..),
 )
import Skeletest.Internal.TestRunner (TestResult)
import Skeletest.Internal.TestTargets (TestTarget, matchesTest)
import Skeletest.Internal.TestTargets qualified as TestTargets
import Skeletest.Prop.Internal (Property)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

data SpecTree
  = SpecTree_Group
      { label :: Text
      , trees :: [SpecTree]
      }
  | SpecTree_Test SpecTest

data SpecTest = SpecTest
  { name :: Text
  , markers :: [SomeMarker]
  -- ^ Markers, in order from least to most recently applied.
  --
  -- >>> withMarker MarkerA . withMarker MarkerB $ test ...
  --
  -- will contain
  --
  -- >>> SpecTree_Test { testMarkers = [MarkerA, MarkerB] }
  , action :: IO TestResult
  }

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

withSpecTrees :: (Monad m) => ([SpecTree] -> m [SpecTree]) -> Spec -> m Spec
withSpecTrees f = fmap (Spec . tell) . f . getSpecTrees

-- | Traverse the tree with the given processing function.
--
-- To preprocess trees with @pre@ and postprocess with @post@:
--
-- >>> traverseSpecTrees (\go -> post <=< mapM go <=< pre) spec
traverseSpecTrees ::
  forall m.
  (Monad m) =>
  ((SpecTree -> m SpecTree) -> [SpecTree] -> m [SpecTree]) ->
  Spec ->
  m Spec
traverseSpecTrees f = withSpecTrees go
 where
  go :: [SpecTree] -> m [SpecTree]
  go = f recurseGroups

  recurseGroups = \case
    group@SpecTree_Group{} -> do
      trees' <- go group.trees
      pure group{trees = trees'}
    stest@SpecTree_Test{} -> pure stest

-- | Map the tree with the given processing function.
--
-- To preprocess trees with @pre@ and postprocess with @post@:
--
-- >>> mapSpecTrees (\go -> post . map go . pre) spec
mapSpecTrees ::
  ((SpecTree -> SpecTree) -> [SpecTree] -> [SpecTree]) ->
  Spec ->
  Spec
mapSpecTrees f = runIdentity . traverseSpecTrees (\go -> pure . f (runIdentity . go))

getSpecTests :: Spec -> [SpecTest]
getSpecTests = Seq.toList . execWriter . traverseSpecTests go
 where
  go stest = tell (Seq.singleton stest) *> pure stest

traverseSpecTests :: (Monad m) => (SpecTest -> m SpecTest) -> Spec -> m Spec
traverseSpecTests f = traverseSpecTrees $ \go ->
  traverse $
    go >=> \case
      group@SpecTree_Group{} -> pure group
      SpecTree_Test test_ -> SpecTree_Test <$> f test_

mapSpecTests :: (SpecTest -> SpecTest) -> Spec -> Spec
mapSpecTests f = runIdentity . traverseSpecTests (pure . f)

filterSpecTests :: (SpecTest -> Bool) -> Spec -> Spec
filterSpecTests f = mapSpecTrees $ \go -> filter f' . map go
 where
  f' = \case
    SpecTree_Group{} -> True
    SpecTree_Test test_ -> f test_

{----- Entrypoint -----}

type SpecRegistry = [SpecInfo]

data SpecInfo = SpecInfo
  { specPath :: FilePath
  , specSpec :: Spec
  }

traverseSpecs :: (Applicative f) => (Spec -> f Spec) -> SpecRegistry -> f SpecRegistry
traverseSpecs f = traverse $ \info -> (\spec -> info{specSpec = spec}) <$> f info.specSpec

mapSpecs :: (Spec -> Spec) -> SpecRegistry -> SpecRegistry
mapSpecs f = runIdentity . traverseSpecs (pure . f)

-- | Remove specs with no tests.
pruneSpec :: SpecRegistry -> SpecRegistry
pruneSpec = mapMaybe $ \info -> do
  let spec = mapSpecTrees (\go -> filter (not . isEmptySpec) . map go) info.specSpec
  guard $ (not . null . getSpecTrees) spec
  pure info{specSpec = spec}
 where
  isEmptySpec = \case
    SpecTree_Group _ [] -> True
    _ -> False

applyTestSelections :: TestTarget -> SpecInfo -> SpecInfo
applyTestSelections selections info = info{specSpec = applySelections info.specSpec}
 where
  applySelections = (`Trans.runReader` []) . traverseSpecTrees apply

  apply go = mapMaybeM $ \case
    group@SpecTree_Group{label} -> Just <$> Trans.local (<> [label]) (go group)
    stest@(SpecTree_Test test_) -> do
      groups <- Trans.ask
      let attrs =
            TestTargets.TestAttrs
              { path = info.specPath
              , identifier = groups <> [test_.name]
              , markers = [Text.pack $ getMarkerName m | SomeMarker m <- test_.markers]
              }
      pure $
        if matchesTest selections attrs
          then Just stest
          else Nothing

  mapMaybeM f = fmap catMaybes . mapM f

{----- Defining a Spec -----}

-- | The entity or concept being tested.
describe :: String -> Spec -> Spec
describe name = runIdentity . withSpecTrees (pure . (: []) . mkGroup)
 where
  mkGroup trees =
    SpecTree_Group
      { label = Text.pack name
      , trees
      }

test :: (Testable m) => String -> m () -> Spec
test name t = Spec $ tell [mkTest]
 where
  mkTest =
    SpecTree_Test $
      SpecTest
        { name = Text.pack name
        , markers = []
        , action = runTestable t
        }

-- | Define an IO-based test.
--
-- Should typically be written to be read as full sentences in traditional BDD style:
-- https://en.wikipedia.org/wiki/Behavior-driven_development.
--
-- @
-- describe \"User\" $ do
--   it "can be checked for equality" $ do
--     user1 `shouldBe` user1
-- @
it :: String -> IO () -> Spec
it = test

-- | Define a property test.
--
-- @
-- describe \"User\" $ do
--   prop "decode . encode === Just" $ do
--     let genUser = ...
--     (decode . encode) P.=== Just \`shouldSatisfy\` P.isoWith genUser
-- @
prop :: String -> Property -> Spec
prop = test

{----- Modifiers -----}

-- | Mark the given spec as expected to fail with the given description.
-- Fails tests if they unexpectedly pass.
--
-- Can be selected with the marker @@xfail@
xfail :: String -> Spec -> Spec
xfail = withMarker . MarkerXFail . Text.pack

-- | Skip all tests in the given spec with the given description.
--
-- Can be selected with the marker @@skip@
skip :: String -> Spec -> Spec
skip = withMarker . MarkerSkip . Text.pack

-- | If at least one test is focused, skip all unfocused tests.
--
-- This definition includes a WARNING so that CI errors if it's accidentally
-- committed (assuming CI runs with @-Wall -Werror@).
--
-- @since 0.3.4
focus :: Spec -> Spec
focus = withMarker MarkerFocus
{-# WARNING in "x-focused-tests" focus "focus should only be used in development" #-}

-- | Mark tests as tests that should only be run when explicitly specified on the command line.
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

data MarkerFocus = MarkerFocus
  deriving (Show)

instance IsMarker MarkerFocus where
  getMarkerName _ = "focus"

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
    group@SpecTree_Group{} -> group
    SpecTree_Test test_ -> SpecTree_Test test_{markers = marker : test_.markers}

-- | Adds the given names as plain markers to all tests in the given spec.
--
-- See 'getMarkerName'.
withMarkers :: [String] -> Spec -> Spec
withMarkers = foldr (\name acc -> withMarker (AnonMarker name) . acc) id
