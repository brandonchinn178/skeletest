module Skeletest.Internal.Utils.Color (
  green,
  red,
  yellow,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.Console.ANSI qualified as ANSI

withANSI :: [ANSI.SGR] -> Text -> Text
withANSI codes s = Text.pack (ANSI.setSGRCode codes) <> s <> Text.pack (ANSI.setSGRCode [ANSI.Reset])

green :: Text -> Text
green = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]

red :: Text -> Text
red = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]

yellow :: Text -> Text
yellow = withANSI [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
