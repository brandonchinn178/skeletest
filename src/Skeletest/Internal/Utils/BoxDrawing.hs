{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Utils.BoxDrawing (
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
drawBox' width boxContents = Text.intercalate "\n" $ [header] <> map go boxContents <> [footer]
 where
  header = "╔" <> Text.replicate (width - 2) "═" <> "╗"
  footer = "╚" <> Text.replicate (width - 2) "═" <> "╝"

  go = \case
    BoxHeader s -> rpad (width - 2) "─" ("╟─[ " <> s <> " ]") <> "─╢"
    BoxText s ->
      Text.intercalate "\n" $
        [ rpad (width - 2) " " ("║ " <> line) <> " ║"
        | rawLine <- Text.lines s
        , line <- if Text.null rawLine then [""] else Text.chunksOf (width - 4) rawLine
        ]

  rpad n fill s = s <> Text.replicate (n - Text.length s) fill
