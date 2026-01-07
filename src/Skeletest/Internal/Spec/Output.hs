{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.Output (
  reportGroup,
  reportTestInProgress,
  reportTestResultInline,
  reportTestResultBox,
  renderPrettyFailure,
  BoxSpec,
  BoxSpecContent (..),
  drawBox,
  IndentLevel,
  indent,
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.IO qualified as IO
import UnliftIO.Exception (SomeException, try)

reportGroup :: IndentLevel -> Text -> IO ()
reportGroup lvl name = do
  Text.putStrLn $ indent lvl name

reportTestInProgress :: IndentLevel -> Text -> IO ()
reportTestInProgress lvl testName = do
  Text.putStr $ indent lvl (testName <> ": ")
  IO.hFlush IO.stdout

reportTestResultInline :: IndentLevel -> Text -> IO ()
reportTestResultInline lvl testResultMessage = do
  Text.putStrLn $ indent (lvl + 1) testResultMessage

reportTestResultBox :: Int -> Text -> BoxSpec -> IO ()
reportTestResultBox width testResultLabel box = do
  Text.putStrLn testResultLabel
  Text.putStrLn $ drawBox width box

type IndentLevel = Int

indent :: IndentLevel -> Text -> Text
indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

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
      try (Text.readFile path) >>= \case
        Right srcFile -> pure $ getLineNum lineNum srcFile
        Left (_ :: SomeException) -> pure Nothing
    let (srcLine, pointerLine) =
          case mLine of
            Just line ->
              ( line
              , Text.replicate (startCol - 1) " " <> Text.replicate (endCol - startCol) "^"
              )
            Nothing ->
              ( "<unknown line>"
              , ""
              )

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

drawBox :: Int -> BoxSpec -> Text
drawBox width boxContents = Text.intercalate "\n" $ [header] <> concatMap draw boxContents <> [footer]
 where
  header = "╔" <> Text.replicate (width - 2) "═" <> "╗"
  footer = "╚" <> Text.replicate (width - 2) "═" <> "╝"

  draw = \case
    BoxHeader s ->
      [ drawLine ""
      , "╟─" <> cpad (width - 4) "─" ("⟨ " <> s <> " ⟩") <> "─╢"
      ]
    BoxText s ->
      [ drawLine line
      | rawLine <- Text.lines s
      , line <- if Text.null rawLine then [""] else Text.chunksOf (width - 4) rawLine
      ]
  drawLine s = "║ " <> rpad (width - 4) " " s <> " ║"

  rpad n fill s = s <> Text.replicate (n - Text.length s) fill
  cpad n fill s =
    let total = n - Text.length s
        left = total `div` 2
        right = total - left
     in Text.replicate left fill <> s <> Text.replicate right fill
