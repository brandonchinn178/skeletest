{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Skeletest.Internal.Utils.Term (
  init,
  setANSISupport,

  -- * Global attributes
  Handle (..),
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
) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.IO.Handle qualified as IO
import System.Console.ANSI qualified as ANSI
import System.Console.Terminal.Size qualified as TermSize
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (init)

data GlobalTermData = GlobalTermData
  { width :: Int
  , stdout :: Handle
  , stderr :: Handle
  }

data Handle = Handle
  { handle :: IO.Handle
  , supportsANSI :: IORef Bool
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
    supportsANSI <- newIORef =<< ANSI.hSupportsANSI handle
    pure Handle{..}
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
supportsANSI handle = readIORef handle.supportsANSI

setANSISupport :: Bool -> IO ()
setANSISupport x = do
  writeIORef globalTermData.stdout.supportsANSI x
  writeIORef globalTermData.stderr.supportsANSI x

flush :: IO ()
flush = do
  IO.hFlush globalTermData.stdout.handle
  IO.hFlush globalTermData.stderr.handle

output :: Text -> IO ()
output = Text.hPutStrLn globalTermData.stdout.handle

outputN :: Text -> IO ()
outputN = hPutStrFlush globalTermData.stdout.handle

outputErr :: Text -> IO ()
outputErr = Text.hPutStrLn globalTermData.stderr.handle

outputErrN :: Text -> IO ()
outputErrN = hPutStrFlush globalTermData.stderr.handle

hPutStrFlush :: IO.Handle -> Text -> IO ()
hPutStrFlush h s = Text.hPutStr h s *> IO.hFlush h
