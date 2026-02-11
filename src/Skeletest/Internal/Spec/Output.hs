{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.Output (
  reportGroup,
  reportTestInProgress,
  reportTestResultWithoutMessage,
  reportTestResultWithInlineMessage,
  reportTestResultWithBoxMessage,
  BoxSpec,
  BoxSpecContent (..),
  IndentLevel,
  indent,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Console.Terminal.Size qualified as Term
import System.IO qualified as IO

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
