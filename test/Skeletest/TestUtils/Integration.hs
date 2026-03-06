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

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Default (Default (..))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Records (HasField (..))
import Skeletest
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Process qualified as Process
import UnliftIO.Exception (handleJust)

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
        }

instance HasField "setMainFile" TestRunner (FileContents -> IO ()) where
  getField runner contents =
    modifyIORef runner.settingsRef $ \settings ->
      settings{mainFileContents = contents}

instance HasField "addTestFile" TestRunner (FilePath -> FileContents -> IO ()) where
  getField runner fp contents = do
    let path = runner.dir </> fp
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (unlines contents)

instance HasField "lookupTestFile" TestRunner (FilePath -> IO (Maybe Text)) where
  getField runner fp =
    handleDNE (\_ -> pure Nothing) . fmap Just $
      runner.readTestFile fp
   where
    handleDNE = handleJust (\e -> guard (isDoesNotExistError e) *> Just e)

instance HasField "readTestFile" TestRunner (FilePath -> IO Text) where
  getField runner fp = Text.readFile $ runner.dir </> fp

data TestArgs = TestArgs
  { cwd :: Maybe FilePath
  , cliArgs :: [String]
  , ghcArgs :: [String]
  , mainFile :: String
  }

instance Default TestArgs where
  def =
    TestArgs
      { cwd = Nothing
      , cliArgs = []
      , ghcArgs = []
      , mainFile = "Main.hs"
      }

instance HasField "runTests" TestRunner (IO (ExitCode, String, String)) where
  getField runner = runner.runTestsWith def

instance HasField "runTestsWith" TestRunner (TestArgs -> IO (ExitCode, String, String)) where
  getField runner args = do
    settings <- readIORef runner.settingsRef
    runner.addTestFile args.mainFile settings.mainFileContents

    let ghcArgs =
          concat
            [ ["-hide-all-packages"]
            , ["-F", "-pgmF=skeletest-preprocessor"]
            , ["-package skeletest"]
            , ["-o", "test-runner"]
            , args.ghcArgs
            , [args.mainFile]
            ]
    runProcWith id "ghc" ghcArgs >>= \case
      (ExitSuccess, _, _) ->
        runProcWith
          (maybe id setCWD args.cwd)
          (runner.dir </> "test-runner")
          args.cliArgs
      result -> pure result
   where
    setCWD dir p = p{Process.cwd = Just dir}
    runProcWith f cmd args_ = do
      let proc = f . setCWD runner.dir $ Process.proc cmd args_
      (code, stdout, stderr) <- Process.readCreateProcessWithExitCode proc ""
      pure (code, sanitize stdout, sanitize stderr)

    sanitize =
      Text.unpack
        . scrubDurations
        . stripOverwrites
        . stripControlChars
        . Text.strip
        . Text.pack
    scrubDurations s =
      case Text.break isDigit s of
        (pre, post)
          -- Check if it's of the form `#.##s`
          | (duration, post') <- Text.splitAt 5 post
          , [c0, '.', c1, c2, 's'] <- Text.unpack duration
          , all isDigit [c0, c1, c2] ->
              pre <> "0.00s" <> scrubDurations post'
          -- Consume the digit, continue on the rest of the string
          | Just (c, post') <- Text.uncons post ->
              pre <> Text.cons c (scrubDurations post')
          -- 'post' is empty; we've checked the whole string
          | otherwise ->
              pre
    stripOverwrites s =
      case Text.breakOn "\r" s of
        (_, "") -> s
        (pre, post) -> Text.dropWhileEnd (/= '\n') pre <> stripOverwrites (Text.drop 1 post)
    stripControlChars s =
      case Text.breakOn "\x1b" s of
        (_, "") -> s
        (pre, post) -> pre <> stripControlChars (Text.drop 1 . Text.dropWhile (/= 'm') $ post)

expectCode :: (HasCallStack) => Int -> IO (ExitCode, String, String) -> IO (String, String)
expectCode expected m = do
  (code, stdout, stderr) <- m
  context (unlines ["===== stdout =====", stdout, "===== stderr =====", stderr]) $
    code `shouldBe` expectedCode
  pure (stdout, stderr)
 where
  expectedCode = if expected == 0 then ExitSuccess else ExitFailure expected

expectSuccess :: (HasCallStack) => IO (ExitCode, String, String) -> IO (String, String)
expectSuccess = expectCode 0

expectFailure :: (HasCallStack) => IO (ExitCode, String, String) -> IO (String, String)
expectFailure = expectCode 1
