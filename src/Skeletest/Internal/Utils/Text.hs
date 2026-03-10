{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Utils.Text (
  showT,
  pluralize,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- TODO: Remove when Text.show is available in the version of 'text' that's
-- shipped with the lowest version of GHC we support.
showT :: (Show a) => a -> Text
showT = Text.pack . show

pluralize :: (Num a, Eq a, Show a) => a -> Text -> Text
pluralize n item = showT n <> " " <> item <> (if n == 1 then "" else "s")
