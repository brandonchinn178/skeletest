{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Capture (
  captureOutputPlugin,
  FixtureCapturedOutput (..),

  -- * CLI flag
  CaptureOutputFlag (..),
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Handle qualified as IO
import Skeletest.Internal.CLI (
  FlagSpec (..),
  FormatFlag (..),
  IsFlag (..),
  getFlag,
  getFormatFlag,
 )
import Skeletest.Internal.CLI qualified as CLI
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureSkeletestTmpDir (..),
  getFixture,
  noCleanup,
  withCleanup,
 )
import Skeletest.Internal.Spec.Output (BoxSpecContent (..))
import Skeletest.Internal.TestRunner (
  TestResult (..),
  TestResultMessage (..),
 )
import Skeletest.Plugin (Hooks (..), Plugin (..), defaultHooks, defaultPlugin)
import System.Directory (removePathForcibly)
import System.IO qualified as IO
import UnliftIO.Exception (finally)

captureOutputPlugin :: Plugin
captureOutputPlugin =
  defaultPlugin
    { cliFlags = [CLI.flag @CaptureOutputFlag]
    , hooks = captureOutputHooks
    }

captureOutputHooks :: Hooks
captureOutputHooks =
  defaultHooks
    { runTest = \_ getResult -> do
        (output, result) <- withCaptureOutput getResult
        addCapturedOutput output result
    }

newtype CaptureOutputFlag = CaptureOutputFlag Bool

instance IsFlag CaptureOutputFlag where
  flagName = "capture-output"
  flagHelp = "Whether to capture stdout/stderr: on (default), off"
  flagSpec =
    OptionalFlag
      { default_ = CaptureOutputFlag True
      , parse = \case
          "off" -> Right $ CaptureOutputFlag False
          "on" -> Right $ CaptureOutputFlag True
          s -> Left $ "invalid value: " <> s
      }

type CapturedOutput = Maybe (Text, Text)

withCaptureOutput :: IO a -> IO (CapturedOutput, a)
withCaptureOutput action = do
  CaptureOutputFlag output <- getFlag
  if output
    then do
      handles <- getFixture @FixtureCapturedOutputHandles
      (stdout, (stderr, a)) <- capture handles.stdout . capture handles.stderr $ action
      pure (Just (stdout, stderr), a)
    else (Nothing,) <$> action
 where
  capture handle m =
    withRestore handle.real $ do
      IO.hFlush handle.real
      IO.hDuplicateTo handle.log handle.real
      a <- m
      out <- getOutput handle
      pure (out, a)

  withRestore h m = do
    buf <- IO.hGetBuffering h
    orig <- IO.hDuplicate h
    m `finally` do
      IO.hDuplicateTo orig h
      IO.hSetBuffering h buf
      IO.hClose orig

addCapturedOutput :: CapturedOutput -> TestResult -> IO TestResult
addCapturedOutput mCapturedOutput result = do
  format <- getFormatFlag
  let output = maybe [] renderOutput mCapturedOutput
  pure $
    if shouldShowOutput format output
      then result{testResultMessage = addOutput output result.testResultMessage}
      else result
 where
  renderOutput (stdout, stderr) =
    concat
      [ renderSection "Captured stdout" stdout
      , renderSection "Captured stderr" stderr
      ]
  renderSection name s =
    if Text.null s
      then []
      else [BoxHeader name, BoxText $ Text.stripEnd s]

  shouldShowOutput format output
    | null output = False
    | format == FormatFlag_Verbose = True
    | result.testResultSuccess = False
    | otherwise = True

  addOutput output resultMessage =
    TestResultMessageBox $ toBoxContents resultMessage <> output
  toBoxContents = \case
    TestResultMessageNone -> []
    TestResultMessageInline msg -> [BoxText msg]
    TestResultMessageBox box -> box

data FixtureCapturedOutputHandles = FixtureCapturedOutputHandles
  { stdout :: LogHandle
  , stderr :: LogHandle
  }

data LogHandle = LogHandle
  { log :: IO.Handle
  , real :: IO.Handle
  }

initHandle ::
  IO.Handle ->
  FilePath ->
  FilePath ->
  IO (LogHandle, IO ())
initHandle real tmpdir file = do
  (fp, h) <- IO.openTempFile tmpdir file
  IO.hSetBuffering h IO.LineBuffering
  let handle = LogHandle{log = h, real = real}
      cleanup = IO.hClose h >> removePathForcibly fp
  pure (handle, cleanup)

instance Fixture FixtureCapturedOutputHandles where
  fixtureAction = do
    FixtureSkeletestTmpDir tmpdir <- getFixture
    (stdout, cleanupStdout) <- initHandle IO.stdout tmpdir "stdout"
    (stderr, cleanupStderr) <- initHandle IO.stderr tmpdir "stderr"
    pure . withCleanup FixtureCapturedOutputHandles{..} $ do
      cleanupStdout
      cleanupStderr

getOutput :: LogHandle -> IO Text
getOutput = readOutputFrom 0

readOutput :: LogHandle -> IO Text
readOutput handle = do
  pos <- IO.hTell handle.log
  readOutputFrom pos handle

readOutputFrom :: Integer -> LogHandle -> IO Text
readOutputFrom n handle = do
  -- Flush buffers
  IO.hFlush handle.real
  IO.hFlush handle.log

  -- Force handle to end of file, to refresh from real handle
  IO.hSeek handle.log IO.SeekFromEnd 0

  IO.hSeek handle.log IO.AbsoluteSeek n
  go ""
 where
  go acc = do
    out <- Text.hGetChunk handle.log
    if Text.null out
      then pure acc
      else go $! acc <> out

-- | Fixture for inspecting the captured output.
--
-- Intended to be used with @OverloadedRecordDot@:
--
-- @
-- output <- getFixture @FixtureCapturedOutput
--
-- -- Read all of stdout/stderr so far
-- stdout <- output.getStdout
-- stderr <- output.getStderr
--
-- -- Read everything in stdout/stderr since the last read
-- stdout_chunk <- output.readStdout
-- stderr_chunk <- output.readStderr
-- @
data FixtureCapturedOutput = FixtureCapturedOutput
  { getStdout :: IO Text
  , getStderr :: IO Text
  , readStdout :: IO Text
  , readStderr :: IO Text
  }

instance Fixture FixtureCapturedOutput where
  fixtureAction = do
    handles <- getFixture @FixtureCapturedOutputHandles
    pure . noCleanup $
      FixtureCapturedOutput
        { getStdout = getOutput handles.stdout
        , getStderr = getOutput handles.stderr
        , readStdout = readOutput handles.stdout
        , readStderr = readOutput handles.stderr
        }
