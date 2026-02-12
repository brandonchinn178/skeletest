module Skeletest.Internal.Paths (
  setOriginalDirectory,
  readTestFile,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Skeletest.Internal.Error (invariantViolation)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

originalDirectoryRef :: IORef FilePath
originalDirectoryRef = unsafePerformIO $ newIORef (invariantViolation "Original directory not set")
{-# NOINLINE originalDirectoryRef #-}

setOriginalDirectory :: FilePath -> IO ()
setOriginalDirectory = writeIORef originalDirectoryRef

readTestFile :: FilePath -> IO Text
readTestFile fp = do
  dir <- readIORef originalDirectoryRef
  Text.readFile $ dir </> fp
