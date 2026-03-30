{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.Exit (
  TestExitCode (..),
  exitWith,
  handleUnknownErrors,
) where

import Data.Text qualified as Text
import Skeletest.Internal.Utils.Term qualified as Term
import System.Exit qualified as Exit
import UnliftIO.Exception (
  displayException,
  fromException,
  handleJust,
 )

-- | All exit codes for Skeletest.
--
-- Should be kept in sync with README.
data TestExitCode
  = ExitSuccess
  | ExitTestFailure
  | ExitNoTests
  | ExitCLIFailure
  | ExitOutdatedSnapshots
  | ExitPreprocessorFailure
  | ExitOther

fromExitCode :: TestExitCode -> Exit.ExitCode
fromExitCode = \case
  ExitSuccess -> Exit.ExitSuccess
  ExitTestFailure -> Exit.ExitFailure 1
  ExitNoTests -> Exit.ExitFailure 3
  ExitCLIFailure -> Exit.ExitFailure 4
  ExitOutdatedSnapshots -> Exit.ExitFailure 5
  ExitPreprocessorFailure -> Exit.ExitFailure 10
  ExitOther -> Exit.ExitFailure 99

exitWith :: TestExitCode -> IO a
exitWith = Exit.exitWith . fromExitCode

handleUnknownErrors :: IO a -> IO a
handleUnknownErrors = handleJust isUnknown $ \e -> do
  Term.outputErr $ (Text.pack . displayException) e
  exitWith ExitOther
 where
  isUnknown e =
    case fromException e of
      Nothing -> Just e
      -- Don't catch 'exitWith' calls
      Just (_ :: Exit.ExitCode) -> Nothing
