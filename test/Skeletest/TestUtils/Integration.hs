{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.TestUtils.Integration (
  integration,

  -- * Test runner
  TestRunner,
  FileContents,

  -- * runTests
  TestArgs (..),
  expectCode,
  expectSuccess,
  expectFailure,

  -- * Re-exports
  ExitCode (..),
  def,
) where

import Data.Default (Default (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text qualified as Text
import GHC.Records (HasField (..))
import Skeletest
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process qualified as Process

data MarkerIntegration = MarkerIntegration
  deriving (Show)

instance IsMarker MarkerIntegration where
  getMarkerName _ = "integration"

integration :: Spec -> Spec
integration = markManual . withMarker MarkerIntegration

{----- runTests -----}

data TestRunner = TestRunner
  { dir :: FilePath
  , settingsRef :: IORef TestRunnerSettings
  }

data TestRunnerSettings = TestRunnerSettings
  { mainFileContents :: FileContents
  , testFiles :: [(FilePath, FileContents)]
  }

-- | File contents as a list of lines.
type FileContents = [String]

instance Fixture TestRunner where
  fixtureAction = do
    FixtureTmpDir dir <- getFixture
    settingsRef <- newIORef defaultSettings
    pure $ noCleanup TestRunner{..}
   where
    defaultSettings =
      TestRunnerSettings
        { mainFileContents = ["import Skeletest.Main"]
        , testFiles = []
        }

instance HasField "setMainFile" TestRunner (FileContents -> IO ()) where
  getField runner contents =
    modifyIORef runner.settingsRef $ \settings ->
      settings{mainFileContents = contents}

instance HasField "addTestFile" TestRunner (FilePath -> FileContents -> IO ()) where
  getField runner fp contents =
    modifyIORef runner.settingsRef $ \settings ->
      settings{testFiles = (fp, contents) : settings.testFiles}

instance HasField "readTestFile" TestRunner (FilePath -> IO String) where
  getField runner fp = readFile $ runner.dir </> fp

data TestArgs = TestArgs
  { cliArgs :: [String]
  , ghcArgs :: [String]
  , mainFile :: String
  }

instance Default TestArgs where
  def =
    TestArgs
      { cliArgs = []
      , ghcArgs = []
      , mainFile = "Main.hs"
      }

instance HasField "runTests" TestRunner (IO (ExitCode, String, String)) where
  getField runner = runner.runTestsWith def

instance HasField "runTestsWith" TestRunner (TestArgs -> IO (ExitCode, String, String)) where
  getField runner args = do
    settings <- readIORef runner.settingsRef
    addFile args.mainFile settings.mainFileContents
    mapM_ (uncurry addFile) settings.testFiles

    let ghcArgs =
          concat
            [ ["-hide-all-packages"]
            , ["-F", "-pgmF=skeletest-preprocessor"]
            , ["-package skeletest"]
            , ["-o", "test-runner"]
            , args.ghcArgs
            , [args.mainFile]
            ]
    runProc "ghc" ghcArgs >>= \case
      (ExitSuccess, _, _) -> runProc "./test-runner" args.cliArgs
      result -> pure result
   where
    addFile fp contents = do
      let path = runner.dir </> fp
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (unlines contents)

    runProc cmd args_ = do
      let proc = (Process.proc cmd args_){Process.cwd = Just runner.dir}
      (code, stdout, stderr) <- Process.readCreateProcessWithExitCode proc ""
      pure (code, sanitize stdout, sanitize stderr)

    sanitize = Text.unpack . stripOverwrites . stripControlChars . Text.strip . Text.pack
    stripOverwrites s =
      case Text.breakOn "\r" s of
        (_, "") -> s
        (pre, post) -> Text.dropWhileEnd (/= '\n') pre <> stripOverwrites (Text.drop 1 post)
    stripControlChars s =
      case Text.breakOn "\x1b" s of
        (_, "") -> s
        (pre, post) -> pre <> stripControlChars (Text.drop 1 . Text.dropWhile (/= 'm') $ post)

expectCode :: (HasCallStack) => ExitCode -> IO (ExitCode, String, String) -> IO (String, String)
expectCode expected m = do
  (code, stdout, stderr) <- m
  context (unlines ["===== stdout =====", stdout, "===== stderr =====", stderr]) $
    code `shouldBe` expected
  pure (stdout, stderr)

expectSuccess :: (HasCallStack) => IO (ExitCode, String, String) -> IO (String, String)
expectSuccess = expectCode ExitSuccess

expectFailure :: (HasCallStack) => IO (ExitCode, String, String) -> IO (String, String)
expectFailure = expectCode $ ExitFailure 1
