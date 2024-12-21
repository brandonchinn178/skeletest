module Skeletest.Internal.Utils.Diff (
  showLineDiff,
) where

import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.Algorithm.DiffContext qualified as Diff
import Data.Text (Text)
import Data.Text qualified as Text
import Text.PrettyPrint qualified as PP

showLineDiff :: (Text, Text) -> (Text, Text) -> Text
showLineDiff (fromName, fromContent) (toName, toContent) =
  Text.pack . PP.render $
    prettyContextDiff (ppText fromName) (ppText toName) (ppText . Diff.unnumber) $
      getContextDiff (Just 5) (Text.lines fromContent) (Text.lines toContent)
  where
    ppText = PP.text . Text.unpack
