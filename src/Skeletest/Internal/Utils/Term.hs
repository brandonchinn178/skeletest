{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Skeletest.Internal.Utils.Term (
  init,
  setANSISupport,

  -- * Global attributes
  Handle,
  width,
  stdout,
  stderr,
  supportsANSI,

  -- * Output helpers
  flush,
  output,
  outputN,
  outputErr,
  outputErrN,
  clearInPlace,
  outputInPlace,
) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import Data.Foldable (traverse_)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Handle qualified as IO
import System.Console.ANSI qualified as ANSI
import System.Console.Terminal.Size qualified as TermSize
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Prelude hiding (init)

data GlobalTermData = GlobalTermData
  { width :: Int
  , stdout :: Handle
  , stderr :: Handle
  }

newtype Handle = Handle {var :: MVar Handle'}

data Handle' = Handle'
  { handle :: IO.Handle
  , supportsANSI :: Bool
  , lastInPlaceOutput :: Maybe Text
  -- ^ See 'outputInPlace'
  }

globalTermData :: GlobalTermData
globalTermData = unsafePerformIO $ do
  width <- maybe 80 TermSize.width <$> TermSize.size
  stdout <- getHandle IO.stdout
  stderr <- getHandle IO.stderr
  pure GlobalTermData{..}
 where
  getHandle h = do
    handle <- IO.hDuplicate h
    supportsANSI <- ANSI.hSupportsANSI handle
    let lastInPlaceOutput = Nothing
    Handle <$> newMVar Handle'{..}
{-# NOINLINE globalTermData #-}

init :: IO ()
init = do
  -- Configure stdout/stderr globally, both for Term.output and for anything
  -- writing directly to stdout/stderr (e.g. user tests with --capture-output=off)
  forM_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering

  -- Make sure globalTermData is initialized
  _ <- evaluate globalTermData
  pure ()

-- Use the terminal width at the beginning of the test suite; don't
-- handle users changing terminal width in the middle right now
width :: Int
width = globalTermData.width

stdout :: Handle
stdout = globalTermData.stdout

stderr :: Handle
stderr = globalTermData.stderr

supportsANSI :: Handle -> IO Bool
supportsANSI (Handle hvar) = (.supportsANSI) <$> readMVar hvar

setANSISupport :: Bool -> IO ()
setANSISupport x =
  forM_ [globalTermData.stdout, globalTermData.stderr] $ \(Handle hvar) -> do
    modifyMVar_ hvar $ \h -> pure h{supportsANSI = x}

flush :: IO ()
flush = do
  forM_ [globalTermData.stdout, globalTermData.stderr] $ \(Handle hvar) -> do
    withMVar hvar $ \h -> IO.hFlush h.handle

output :: Text -> IO ()
output = outputWith globalTermData.stdout

outputN :: Text -> IO ()
outputN = outputNWith globalTermData.stdout

outputErr :: Text -> IO ()
outputErr = outputWith globalTermData.stderr

outputErrN :: Text -> IO ()
outputErrN = outputNWith globalTermData.stderr

outputWith :: Handle -> Text -> IO ()
outputWith (Handle hvar) s = modifyMVar_ hvar $ \h -> outputWith' h s

outputNWith :: Handle -> Text -> IO ()
outputNWith (Handle hvar) s = modifyMVar_ hvar $ \h -> outputNWith' h s

outputWith' :: Handle' -> Text -> IO Handle'
outputWith' h s = do
  Text.hPutStrLn h.handle s
  pure h{lastInPlaceOutput = Nothing}

outputNWith' :: Handle' -> Text -> IO Handle'
outputNWith' h s = do
  Text.hPutStr h.handle s
  IO.hFlush h.handle
  pure h{lastInPlaceOutput = Nothing}

-- | Same as 'outputN', except if called multiple times in a row, will update
-- the message in-place.
--
-- Should only be used when stdout supports ANSI, but this isn't checked.
--
-- Long-term, should probably be replaced with concurrent-output, but it's
-- currently hardcoded to IO.stdout.
outputInPlace :: Text -> IO ()
outputInPlace s = modifyMVar_ globalTermData.stdout.var $ \h0 -> do
  h1 <- clearInPlaceWith' h0
  h2 <- outputNWith' h1 s
  pure h2{lastInPlaceOutput = Just s}

-- | Clear last in-place output. See 'outputInPlace'.
clearInPlace :: IO ()
clearInPlace = modifyMVar_ globalTermData.stdout.var $ \h -> clearInPlaceWith' h

clearInPlaceWith' :: Handle' -> IO Handle'
clearInPlaceWith' h = do
  traverse_ clearLastInPlaceOutput h.lastInPlaceOutput
  pure h{lastInPlaceOutput = Nothing}
 where
  clearLastInPlaceOutput lastOutput = do
    let n = calculateNumLines lastOutput
    Text.hPutStr h.handle "\r\ESC[K" -- Clear the current line
    replicateM_ (n - 1) $ do
      -- Clear any lines above
      Text.hPutStr h.handle "\ESC[F\ESC[K"

  calculateNumLines =
    let calc = length . Text.chunksOf globalTermData.width . stripAnsiEscapeCodes
     in sum . map calc . Text.lines
