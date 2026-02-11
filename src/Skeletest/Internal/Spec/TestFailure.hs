{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.TestFailure (
  TestSrcs (..),
  setTestSrcs,
  renderPrettyFailure,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (SomeException, try)

data TestSrcs
  = TestSrcs_FromDisk
  | TestSrcs_Static [(FilePath, Text)]
  deriving (Show, Read)

readTestSrc :: FilePath -> IO Text
readTestSrc fp =
  readIORef testSrcsRef >>= \case
    TestSrcs_FromDisk -> Text.readFile fp
    TestSrcs_Static files ->
      case lookup fp files of
        Nothing -> mempty -- error doesn't matter, isn't displayed to user
        Just src -> pure src

testSrcsRef :: IORef TestSrcs
testSrcsRef = unsafePerformIO $ newIORef TestSrcs_FromDisk

setTestSrcs :: TestSrcs -> IO ()
setTestSrcs = writeIORef testSrcsRef

-- | Render a test failure like:
--
-- @
-- At test/Skeletest/Internal/TestTargetsSpec.hs:19:
-- |
-- |           parseTestTargets input `shouldBe` Right (Just expected)
-- |                                   ^^^^^^^^
--
-- Right 1 ≠ Left 1
-- @
renderPrettyFailure ::
  -- | Message
  Text ->
  -- | Failure context
  [Text] ->
  -- | Call stack (file, line, startCol, endCol)
  [(FilePath, Int, Int, Int)] ->
  IO Text
renderPrettyFailure msg ctx callstack = do
  prettyStackTrace <- mapM renderCallLine . reverse $ callstack
  pure . Text.intercalate "\n\n" . concat $
    [ prettyStackTrace
    , if null ctx
        then []
        else [Text.intercalate "\n" $ reverse ctx]
    , [msg]
    ]
 where
  renderCallLine (path, lineNum, startCol, endCol) = do
    mLine <-
      try (readTestSrc path) >>= \case
        Right srcFile
          | Just line <- getLineNum lineNum srcFile -> pure $ Right line
          | otherwise -> pure $ Left "<line does not exist>"
        Left (_ :: SomeException) -> pure $ Left "<could not open file>"
    let (srcLine, pointerLine) =
          case mLine of
            Right line ->
              ( line
              , Text.replicate (startCol - 1) " " <> Text.replicate (endCol - startCol) "^"
              )
            Left e -> (e, "")

    pure . Text.intercalate "\n" $
      [ Text.pack path <> ":" <> (Text.pack . show) lineNum <> ":"
      , "│"
      , "│ " <> srcLine
      , "│ " <> pointerLine
      ]

  getLineNum n = listToMaybe . take 1 . drop (n - 1) . Text.lines
