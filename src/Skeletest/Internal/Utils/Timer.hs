{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Utils.Timer (
  withTimer,
  renderDuration,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Numeric (showFFloat)

withTimer :: IO a -> IO (a, NominalDiffTime)
withTimer m = do
  start <- getCurrentTime
  a <- m
  end <- getCurrentTime
  pure (a, end `diffUTCTime` start)

renderDuration :: NominalDiffTime -> Text
renderDuration duration = (Text.pack . showRounded) duration <> "s"
 where
  showRounded n = showFFloat (Just 2) (realToFrac n :: Double) ""
