{-# LANGUAGE OverloadedStrings #-}

{-| This module defines all the constants used in skeletest.

These constants should all be considered arbitrary. Users should
not use any of these identifiers directly.
-}
module Skeletest.Internal.Constants (
  mainFileSpecsListIdentifier,
) where

import Data.Text

-- | The name of the list of Specs collected from test modules
-- in the Main module.
mainFileSpecsListIdentifier :: Text
mainFileSpecsListIdentifier = "skeletest_all_specs"
