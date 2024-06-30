module Skeletest.Internal.Error (
  skeletestPluginError,
  invariantViolation,
) where

import Data.List (dropWhileEnd)
import GHC.Utils.Panic (pgmError)

-- | Throw a user error during compilation, e.g. during the preprocessor or plugin phases.
skeletestPluginError :: String -> a
skeletestPluginError msg =
  pgmError . dropWhileEnd (== '\n') . unlines $
    [ ""
    , "******************** skeletest failure ********************"
    , msg
    ]

-- | Throw an error in a situation that should never happen, and indicates a bug.
invariantViolation :: String -> a
invariantViolation msg =
  error . unlines $
    [ "Invariant violation: " <> msg
    , "**** This is a skeletest bug. Please report it at https://github.com/brandonchinn178/skeletest/issues"
    ]
