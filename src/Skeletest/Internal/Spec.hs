{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  Spec,
  SpecTree (..),
  getSpecTrees,
  filterSpec,
  mapMaybeSpec,
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
  AnonMarker (..),
  withMarker,
  withMarkers,
) where

import Control.Concurrent (myThreadId)
import Control.Monad (forM, guard)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import GHC.Stack qualified as GHC
import System.Console.ANSI qualified as ANSI
import UnliftIO.Exception (
  SomeException,
  displayException,
  finally,
  fromException,
  try,
  trySyncOrAsync,
 )

import Skeletest.Assertions (TestFailure (..))
import Skeletest.Internal.Fixtures (FixtureScopeKey (..), cleanupFixtures)
import Skeletest.Internal.State (TestInfo (..), withTestInfo)
import Skeletest.Internal.TestTargets (TestTargets, matchesTest)
import Skeletest.Internal.TestTargets qualified as TestTargets
import Skeletest.Prop.Internal (Property, runProperty)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

withSpecTrees :: ([SpecTree] -> [SpecTree]) -> Spec -> Spec
withSpecTrees f = Spec . tell . f . getSpecTrees

data SpecTree
  = SpecGroup Text [SpecTree]
  | SpecTest Text (IO ())

-- | Filter specs bottom-to-top.
filterSpec :: (SpecTree -> Bool) -> Spec -> Spec
filterSpec f = mapMaybeSpec (\tree -> if f tree then Just tree else Nothing)

-- | Map + filter specs bottom-to-top.
mapMaybeSpec :: (SpecTree -> Maybe SpecTree) -> Spec -> Spec
mapMaybeSpec f = withSpecTrees go
  where
    go :: [SpecTree] -> [SpecTree]
    go = mapMaybe f . map recurseGroups

    recurseGroups = \case
      SpecGroup name trees -> SpecGroup name (go trees)
      SpecTest name io -> SpecTest name io

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
                , testFile = specPath
                }
        Text.putStrLn specName
        runTrees emptyTestInfo $ getSpecTrees specSpec
  where
    runTrees testInfo = fmap and . mapM (runTree testInfo)
    runTree testInfo = \case
      SpecGroup name trees -> do
        let lvl = getIndentLevel testInfo
        Text.putStrLn $ indent lvl name
        runTrees testInfo{testContexts = testContexts testInfo <> [name]} trees
      SpecTest name io -> do
        let lvl = getIndentLevel testInfo
        Text.putStr $ indent lvl (name <> ": ")

        -- TODO: timeout
        result <-
          trySyncOrAsync $
            withTestInfo testInfo{testName = name} $ do
              tid <- myThreadId
              io `finally` cleanupFixtures (PerTestFixtureKey tid)

        case result of
          Right () -> do
            Text.putStrLn $ green "OK"
            pure True
          Left (e :: SomeException) -> do
            case fromException e of
              Just failure -> do
                Text.putStrLn $ red "FAIL"
                Text.putStrLn =<< renderTestFailure failure
              Nothing -> do
                Text.putStrLn $ red "ERROR"
                Text.putStrLn $ indent lvl (Text.pack $ displayException e)
            pure False

    getIndentLevel testInfo = length (testContexts testInfo) + 1 -- +1 to include the module name
    indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

    withANSI codes s = Text.pack (ANSI.setSGRCode codes) <> s <> Text.pack (ANSI.setSGRCode [ANSI.Reset])
    green = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
    red = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]

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
  let spec = filterSpec (not . isEmptySpec) (specSpec info)
  guard $ (not . null . getSpecTrees) spec
  pure info{specSpec = spec}
  where
    isEmptySpec = \case
      SpecGroup _ [] -> True
      _ -> False

applyTestSelections :: TestTargets -> SpecRegistry -> SpecRegistry
applyTestSelections = \case
  Nothing -> id
  Just selections ->
    map $ \info ->
      let
        goTrees ctx = mapMaybe (goTree ctx)
        goTree ctx = \case
          SpecGroup name trees ->
            pure $ SpecGroup name $ goTrees (ctx <> [name]) trees
          tree@(SpecTest name _) -> do
            -- TODO: specify markers
            let markers = []
            guard . matchesTest selections $
              TestTargets.TestAttrs
                { testPath = specPath info
                , testIdentifier = ctx <> [name]
                , testMarkers = markers
                }
            pure tree
      in
        info{specSpec = withSpecTrees (goTrees []) (specSpec info)}

{----- Defining a Spec -----}

describe :: String -> Spec -> Spec
describe name spec = Spec $ tell [SpecGroup (Text.pack name) (getSpecTrees spec)]

class Testable a where
  runTest :: a -> IO ()

instance Testable (IO ()) where
  runTest = id

instance Testable Property where
  runTest = runProperty

test :: Testable a => String -> a -> Spec
test name t = Spec $ tell [SpecTest (Text.pack name) (runTest t)]

it :: String -> IO () -> Spec
it = test

prop :: String -> Property -> Spec
prop = test

{----- Modifiers -----}

-- | Mark the given spec as expected to fail.
-- Fails tests if they unexpectedly pass.
xfail :: String -> Spec -> Spec
xfail = undefined

-- | Skip all tests in the given spec.
skip :: String -> Spec -> Spec
skip = undefined

{----- Markers -----}

class Typeable a => IsMarker a where
  getMarkerName :: a -> Text

newtype AnonMarker = AnonMarker Text

instance IsMarker AnonMarker where
  getMarkerName (AnonMarker n) = n

-- | Adds the given marker to all the tests in the given spec.
-- Useful for selecting tests from the command line or
-- identifying tests in hooks
withMarker :: IsMarker a => a -> Spec -> Spec
withMarker = undefined

withMarkers :: [String] -> Spec -> Spec
withMarkers = foldr (\mark acc -> withMarker (toAnon mark) . acc) id
  where
    toAnon = AnonMarker . Text.pack
