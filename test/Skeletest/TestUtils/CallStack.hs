{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.TestUtils.CallStack (
  sanitizeTraceback,
) where

#if __GLASGOW_HASKELL__ == 910
import Data.Text qualified as Text
#endif

-- GHC 9.10 specifically added a backtrace to SomeException, which was reverted in 9.12
-- https://github.com/haskell/core-libraries-committee/issues/285
sanitizeTraceback :: String -> String
#if __GLASGOW_HASKELL__ == 910
sanitizeTraceback s =
  let (pre, post) = break ("HasCallStack backtrace:" `Text.isInfixOf`) $ Text.lines $ Text.pack s
      (_, post2) = span (", called at" `Text.isInfixOf`) $ drop 1 post
      post2' = map trimBorder post2
   in Text.unpack . Text.unlines $ pre ++ post2'
 where
  trimBorder line =
    if Text.all (== '─') . Text.drop 1 $ line
      then Text.take 80 line
      else line
#else
sanitizeTraceback = id
#endif
