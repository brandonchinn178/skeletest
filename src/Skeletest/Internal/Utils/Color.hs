module Skeletest.Internal.Utils.Color (
  green,
  red,
  yellow,
  gray,
) where

import Data.Colour.Names qualified as Color
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

gray :: Text -> Text
gray = withANSI [ANSI.SetRGBColor ANSI.Foreground Color.gray]
