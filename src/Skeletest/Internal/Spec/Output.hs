{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec.Output (
  BoxSpec,
  BoxSpecContent (..),
  drawBox,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.Console.Terminal.Size qualified as Term

type BoxSpec = [BoxSpecContent]

data BoxSpecContent
  = BoxText Text
  | BoxHeader Text
  deriving (Show, Eq)

drawBox :: BoxSpec -> IO Text
drawBox box = do
  width <- maybe 80 (max 40 . Term.width) <$> Term.size
  pure $ drawBox' width box

drawBox' :: Int -> BoxSpec -> Text
drawBox' width boxContents = Text.intercalate "\n" $ [header] <> concatMap draw boxContents <> [footer]
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
