{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.Output (
  reportGroup,
  reportTestInProgress,
  reportTestResultWithoutMessage,
  reportTestResultWithInlineMessage,
  reportTestResultWithBoxMessage,
  renderPrettyFailure,
  BoxSpec,
  BoxSpecContent (..),
  IndentLevel,
  indent,
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Skeletest.Internal.Paths (readTestFile)
import System.Console.Terminal.Size qualified as Term
import System.IO qualified as IO
import UnliftIO.Exception (SomeException, try)

reportGroup :: IndentLevel -> Text -> IO ()
reportGroup lvl name = do
  Text.putStrLn $ indent lvl name

reportTestInProgress :: IndentLevel -> Text -> IO ()
reportTestInProgress lvl testName = do
  Text.putStr $ indent lvl (testName <> ": ")
  IO.hFlush IO.stdout

reportTestResultWithoutMessage :: Text -> IO ()
reportTestResultWithoutMessage testResultLabel = do
  Text.putStrLn testResultLabel

reportTestResultWithInlineMessage :: IndentLevel -> Text -> Text -> IO ()
reportTestResultWithInlineMessage lvl testResultLabel testResultMessage = do
  Text.putStrLn testResultLabel
  Text.putStrLn $ indent (lvl + 1) testResultMessage

reportTestResultWithBoxMessage :: Maybe (Term.Window Int) -> IndentLevel -> Text -> Text -> BoxSpec -> IO ()
reportTestResultWithBoxMessage termSize lvl testName testResultLabel box = do
  if null box
    then do
      Text.putStrLn testResultLabel
    else do
      Text.putStr "\r"
      Text.putStrLn $ drawBoxHeader lvl testName <> ": " <> testResultLabel
      Text.putStrLn $ drawBoxBody termSize box

type IndentLevel = Int

indentSize :: Int
indentSize = 4

indentWith :: Text -> IndentLevel -> Text -> Text
indentWith fill lvl = Text.intercalate "\n" . map (Text.replicate (lvl * indentSize) fill <>) . Text.splitOn "\n"

indent :: IndentLevel -> Text -> Text
indent = indentWith " "

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
      try (readTestFile path) >>= \case
        Right srcFile
          | Just line <- getLineNum lineNum srcFile -> pure $ Right line
          | otherwise -> pure $ Left $ "<line does not exist: " <> path <> ":" <> show lineNum <> ">"
        Left (_ :: SomeException) -> pure $ Left $ "<could not open file: " <> path <> ">"
    let (srcLine, pointerLine) =
          case mLine of
            Right line ->
              ( line
              , Text.replicate (startCol - 1) " " <> Text.replicate (endCol - startCol) "^"
              )
            Left e -> (Text.pack e, "")

    pure . Text.intercalate "\n" $
      [ Text.pack path <> ":" <> (Text.pack . show) lineNum <> ":"
      , "│"
      , "│ " <> srcLine
      , "│ " <> pointerLine
      ]

  getLineNum n = listToMaybe . take 1 . drop (n - 1) . Text.lines

type BoxSpec = [BoxSpecContent]

data BoxSpecContent
  = BoxText Text
  | BoxHeader Text
  deriving (Show, Eq)

drawBoxHeader :: IndentLevel -> Text -> Text
drawBoxHeader lvl testName = "╭" <> Text.drop 2 (indentWith "─" lvl "") <> " " <> testName

drawBoxBody :: Maybe (Term.Window Int) -> BoxSpec -> Text
drawBoxBody termSize boxContents = Text.intercalate "\n" $ concatMap draw boxContents <> [footer]
 where
  termWidth = maybe 80 Term.width termSize
  width =
    maximum . (termWidth :) . flip concatMap boxContents $ \case
      BoxHeader s -> [Text.length s + indentSize + 2]
      BoxText s -> [Text.length line + 2 | line <- Text.lines s]

  footer = "╰" <> Text.replicate (width - 1) "─"

  draw = \case
    BoxHeader s ->
      [ drawLine ""
      , "╞═══ " <> s
      ]
    BoxText s -> map drawLine $ Text.lines s
  drawLine s = "│ " <> s
