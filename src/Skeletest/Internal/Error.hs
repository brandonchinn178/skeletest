module Skeletest.Internal.Error (
  skeletestError,
) where

import Data.List (dropWhileEnd)
import GHC.Utils.Panic (pgmError)

skeletestError :: String -> a
skeletestError msg =
  pgmError . dropWhileEnd (== '\n') . unlines $
    [ ""
    , "******************** skeletest failure ********************"
    , msg
    ]
