module Skeletest.Internal.Utils.Diff (
  showLineDiff,
) where

import Data.Algorithm.DiffContext (getContextDiffNew, prettyContextDiff)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.PrettyPrint qualified as PP

showLineDiff :: (Text, Text) -> (Text, Text) -> Text
showLineDiff (fromName, fromContent) (toName, toContent) =
  Text.pack . PP.render $
    prettyContextDiff (ppText fromName) (ppText toName) ppText $
      getContextDiffNew (Just 5) (Text.lines fromContent) (Text.lines toContent)
  where
    ppText = PP.text . Text.unpack
