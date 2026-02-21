{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.Paths (
  setOriginalDirectory,
  readTestFile,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Skeletest.Internal.Error (invariantViolation)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

originalDirectoryRef :: IORef FilePath
originalDirectoryRef = unsafePerformIO $ newIORef (invariantViolation "Original directory not set")
{-# NOINLINE originalDirectoryRef #-}

data TestRoot = TestRootBuildDir | TestRootCWD | TestRoot FilePath

setOriginalDirectory :: FilePath -> IO ()
setOriginalDirectory buildDir = do
  testRoot <-
    lookupEnv "SKELETEST_TEST_ROOT" >>= \case
      Nothing -> pure TestRootBuildDir
      Just "BUILD_DIR" -> pure TestRootBuildDir
      Just "CWD" -> pure TestRootCWD
      Just fp -> pure $ TestRoot fp
  root <-
    case testRoot of
      TestRootBuildDir -> pure buildDir
      TestRootCWD -> getCurrentDirectory
      TestRoot fp -> pure fp
  writeIORef originalDirectoryRef root

readTestFile :: FilePath -> IO Text
readTestFile fp = do
  dir <- readIORef originalDirectoryRef
  Text.readFile $ dir </> fp
