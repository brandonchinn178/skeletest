{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.TestUtils.Integration (
  integration,

  -- * Test runner
  FixtureTestRunner,
  FileContents,
  setMainFile,
  addTestFile,
  readTestFile,

  -- * runTests
  runTests,
  expectSuccess,

  -- * Re-exports
  ExitCode (..),
) where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text qualified as Text
import Skeletest
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

data MarkerIntegration = MarkerIntegration
  deriving (Show)

instance IsMarker MarkerIntegration where
  getMarkerName _ = "integration"

integration :: Spec -> Spec
integration = markManual . withMarker MarkerIntegration

{----- runTests -----}

data FixtureTestRunner = FixtureTestRunner
  { testRunnerDir :: FilePath
  , testRunnerSettingsRef :: IORef TestRunnerSettings
  }

data TestRunnerSettings = TestRunnerSettings
  { mainFile :: FileContents
  , testFiles :: [(FilePath, FileContents)]
  }

-- | File contents as a list of lines.
type FileContents = [String]

instance Fixture FixtureTestRunner where
  fixtureAction = do
    FixtureTmpDir tmpdir <- getFixture
    settingsRef <- newIORef defaultSettings
    pure . noCleanup $
      FixtureTestRunner
        { testRunnerDir = tmpdir
        , testRunnerSettingsRef = settingsRef
        }
    where
      defaultSettings =
        TestRunnerSettings
          { mainFile = ["import Skeletest.Main"]
          , testFiles = []
          }

setMainFile :: FixtureTestRunner -> FileContents -> IO ()
setMainFile FixtureTestRunner{testRunnerSettingsRef} contents =
  modifyIORef testRunnerSettingsRef $ \settings -> settings{mainFile = contents}

addTestFile :: FixtureTestRunner -> FilePath -> FileContents -> IO ()
addTestFile FixtureTestRunner{testRunnerSettingsRef} fp contents =
  modifyIORef testRunnerSettingsRef $ \settings ->
    settings{testFiles = (fp, contents) : testFiles settings}

readTestFile :: FixtureTestRunner -> FilePath -> IO String
readTestFile FixtureTestRunner{testRunnerDir} fp = readFile $ testRunnerDir </> fp

runTests :: FixtureTestRunner -> [String] -> IO (ExitCode, String, String)
runTests FixtureTestRunner{..} args = do
  TestRunnerSettings{..} <- readIORef testRunnerSettingsRef
  addFile "Main.hs" mainFile
  mapM_ (uncurry addFile) testFiles

  (code, stdout, stderr) <-
    flip readCreateProcessWithExitCode "" $
      setCWD testRunnerDir . proc "runghc" . concat $
        [ "--" : ghcArgs
        , "--" : "Main.hs" : args
        ]

  pure (code, sanitize stdout, sanitize stderr)
  where
    addFile fp contents = do
      let path = testRunnerDir </> fp
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (unlines contents)

    ghcArgs =
      concat
        [ ["-hide-all-packages"]
        , ["-F", "-pgmF=skeletest-preprocessor"]
        , ["-package skeletest"]
        ]
    setCWD dir p = p{cwd = Just dir}

    sanitize = Text.unpack . stripControlChars . Text.strip . Text.pack
    stripControlChars s =
      case Text.breakOn "\x1b" s of
        (_, "") -> s
        (pre, post) -> pre <> stripControlChars (Text.drop 1 . Text.dropWhile (/= 'm') $ post)

expectSuccess :: (HasCallStack) => IO (ExitCode, String, String) -> IO (String, String)
expectSuccess m = do
  (code, stdout, stderr) <- m
  context (unlines ["===== stdout =====", stdout, "===== stderr =====", stderr]) $
    code `shouldBe` ExitSuccess
  pure (stdout, stderr)
