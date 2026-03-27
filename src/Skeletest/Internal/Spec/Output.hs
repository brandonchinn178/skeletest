{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.Output (
  -- * Rendering failures
  renderPrettyFailure,

  -- * BoxSpec
  BoxSpec,
  BoxSpecContent (..),
) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest.Internal.Paths (readTestFile)
import Skeletest.Internal.Utils.Text (showT)
import UnliftIO.Exception (SomeException, try)

{----- Rendering failures -----}

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
      [ Text.pack path <> ":" <> showT lineNum <> ":"
      , "│"
      , "│ " <> srcLine
      , "│ " <> pointerLine
      ]

  getLineNum n = listToMaybe . take 1 . drop (n - 1) . Text.lines

{----- BoxSpec -----}

-- | The specification for boxed output.
type BoxSpec = [BoxSpecContent]

data BoxSpecContent
  = BoxText Text
  | BoxHeader Text
  deriving (Show, Eq)
