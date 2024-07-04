{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  Spec,
  SpecTree (..),
  getSpecTrees,
  filterSpec,
  runSpecs,

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

import Control.Monad (forM_)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Typeable (Typeable)
import GHC.Stack qualified as GHC
import UnliftIO.Exception (
  SomeException,
  displayException,
  finally,
  fromException,
  trySyncOrAsync,
 )

import Skeletest.Assertions (TestFailure (..))
import Skeletest.Internal.Fixtures (FixtureScope (..), cleanupFixtures)
import Skeletest.Internal.State (TestInfo (..), withTestInfo)
import Skeletest.Prop.Internal (Property, runProperty)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

data SpecTree
  = SpecGroup Text [SpecTree]
  | SpecTest Text (IO ())

-- | Filter specs bottom-to-top.
filterSpec :: (SpecTree -> Bool) -> Spec -> Spec
filterSpec f = Spec . tell . go . getSpecTrees
  where
    go :: [SpecTree] -> [SpecTree]
    go = filter f . map recurseGroups

    recurseGroups = \case
      SpecGroup name trees -> SpecGroup name (go trees)
      SpecTest name io -> SpecTest name io

-- TODO: allow running tests in parallel
runSpecs :: [(FilePath, Text, Spec)] -> IO ()
runSpecs specs =
  (`finally` cleanupFixtures PerSessionFixture) $
    forM_ specs $ \(testFile, testModule, spec) -> do
      let emptyTestInfo =
            TestInfo
              { testModule
              , testContexts = []
              , testName = ""
              , testFile
              }
      Text.putStrLn testModule
      go emptyTestInfo $ getSpecTrees spec
  where
    -- TODO: colors
    go testInfo = mapM_ $ \case
      SpecGroup name trees -> do
        let lvl = getIndentLevel testInfo
        Text.putStrLn $ indent lvl name
        go testInfo{testContexts = testContexts testInfo <> [name]} trees
      SpecTest name io -> do
        let lvl = getIndentLevel testInfo
        Text.putStr $ indent lvl (name <> ": ")

        -- TODO: timeout
        result <-
          trySyncOrAsync $
            withTestInfo testInfo{testName = name} $ do
              io `finally` cleanupFixtures PerTestFixture

        case result of
          Right () -> do
            Text.putStrLn "OK"
          Left (e :: SomeException)
            | Just TestFailure{testInfo = _, ..} <- fromException e -> do
                Text.putStrLn "FAIL"
                Text.putStrLn $ indent (lvl + 1) testFailMessage
                Text.putStrLn $ indent (lvl + 1) (Text.pack $ GHC.prettyCallStack callStack)
            | otherwise -> do
                Text.putStrLn "ERROR"
                Text.putStrLn $ indent lvl (Text.pack $ displayException e)

    getIndentLevel testInfo = length (testContexts testInfo) + 1 -- +1 to include the module name
    indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

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
