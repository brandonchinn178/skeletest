{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Capture (
  CaptureOutputFlag,
  withCaptureOutput,
  addCapturedOutput,
  FixtureCapturedOutput (..),
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Handle qualified as IO
import Skeletest.Internal.CLI (
  FlagSpec (..),
  IsFlag (..),
  getFlag,
 )
import Skeletest.Internal.Fixtures (
  Fixture (..),
  FixtureSkeletestTmpDir (..),
  getFixture,
  noCleanup,
  withCleanup,
 )
import Skeletest.Internal.TestRunner (
  TestResult (..),
  TestResultMessage (..),
 )
import Skeletest.Internal.Utils.BoxDrawing (BoxSpecContent (..))
import System.Directory (removePathForcibly)
import System.IO qualified as IO
import UnliftIO.Exception (finally)

newtype CaptureOutputFlag = CaptureOutputFlag Bool

instance IsFlag CaptureOutputFlag where
  flagName = "capture-output"
  flagHelp = "Whether to capture stdout/stderr: on (default), off"
  flagSpec =
    OptionalFlag
      { flagDefault = CaptureOutputFlag True
      , flagParse = \case
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

addCapturedOutput :: CapturedOutput -> TestResult -> TestResult
addCapturedOutput = maybe id updateResult
 where
  updateResult output result =
    result
      { testResultMessage =
          TestResultMessageSection . concat $
            [ toBoxContents (testResultMessage result)
            , renderOutput output
            ]
      }
  toBoxContents = \case
    TestResultMessageNone -> []
    TestResultMessageInline msg -> [BoxText msg]
    TestResultMessageSection box -> box
  renderOutput (stdout, stderr) =
    concat
      [ if Text.null stdout then [] else [BoxHeader "Captured stdout", BoxText stdout]
      , if Text.null stderr then [] else [BoxHeader "Captured stderr", BoxText stderr]
      ]

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
