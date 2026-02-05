{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-| A preprocessor that registers skeletest in a test suite.

We need to use a preprocessor for Main.hs because GHC plugins don't
seem to support dynamically registering other modules as imports (GHC
already knows what order it's going to compile the modules in, because
plugins run per module).

But GHC's plugin interface is much nicer for inspecting and manipulating
the code. So what we'll do here is:

1. Always register the plugin by adding `{\-# OPTIONS_GHC -fplugin=... #-\}` to
   the top of the file. The plugin will then inspect the file to see if it's
   a test file or the main file, and if so, process it.

2. If the file is the main file, insert the appropriate imports.
-}
module Main where

import Data.Char (isUpper)
import Data.Foldable (foldlM)
import Data.List (dropWhileEnd)
import Data.Text.IO qualified as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UnliftIO.Exception (displayException, handle)

import Data.Text qualified as Text
import Skeletest.Internal.Error (SkeletestError)
import Skeletest.Internal.Preprocessor (processFile)
import Skeletest.Internal.Preprocessor qualified as Preprocessor

main :: IO ()
main = handleErrors $ do
  -- just to be extra sure we don't run into encoding issues
  setLocaleEncoding utf8

  getArgs >>= \case
    -- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#options-affecting-a-haskell-pre-processor
    fp : input : output : args -> do
      options <- either error pure $ foldlM parseOpts Preprocessor.defaultOptions args
      Text.readFile input >>= processFile options fp >>= Text.writeFile output
    _ -> error "The skeletest preprocessor expects at least three arguments."

-- | Output SkeletestError
handleErrors :: IO a -> IO a
handleErrors = handle $ \(e :: SkeletestError) -> do
  hPutStrLn stderr $ normalizeLines $ displayException e
  exitFailure
 where
  normalizeLines
    | __GLASGOW_HASKELL__ == (908 :: Int) = dropWhileEnd (== '\n')
    | otherwise = id

parseOpts :: Preprocessor.Options -> String -> Either String Preprocessor.Options
parseOpts opts argStr
  | Just (mMainMod, mMainFunc) <- Text.stripPrefix "main:" arg >>= parseMain =
      Right
        . maybe id (\s o -> o{Preprocessor.mainModuleName = s}) mMainMod
        . maybe id (\s o -> o{Preprocessor.mainFuncName = s}) mMainFunc
        $ opts
  | otherwise = Left $ "Unknown option: " <> argStr
 where
  arg = Text.pack argStr
  parseMain s =
    case Text.splitOn "." s of
      [modName, modFunc] -> Just (Just modName, Just modFunc)
      [_] -> do
        (c, _) <- Text.uncons s
        Just $
          if isUpper c
            then (Just s, Nothing)
            else (Nothing, Just s)
      _ -> Nothing
