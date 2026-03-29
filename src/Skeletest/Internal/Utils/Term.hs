{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.Utils.Term (
  init,
  width,
  output,
  outputN,
  outputErr,
  outputErrN,
) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.IO.Handle qualified as IO
import System.Console.Terminal.Size qualified as TermSize
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (init)

data GlobalTermData = GlobalTermData
  { width :: Int
  , stdout :: IO.Handle
  , stderr :: IO.Handle
  }

globalTermData :: GlobalTermData
globalTermData = unsafePerformIO $ do
  width_ <- maybe 80 TermSize.width <$> TermSize.size
  stdout <- IO.hDuplicate IO.stdout
  stderr <- IO.hDuplicate IO.stderr
  pure GlobalTermData{width = width_, ..}
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

output :: Text -> IO ()
output = Text.hPutStrLn globalTermData.stdout

outputN :: Text -> IO ()
outputN = hPutStrFlush globalTermData.stdout

outputErr :: Text -> IO ()
outputErr = Text.hPutStrLn globalTermData.stderr

outputErrN :: Text -> IO ()
outputErrN = hPutStrFlush globalTermData.stderr

hPutStrFlush :: IO.Handle -> Text -> IO ()
hPutStrFlush h s = Text.hPutStr h s *> IO.hFlush h
