{-# LANGUAGE OverloadedStrings #-}

{-| This module defines all the constants used in skeletest.

These constants should all be considered arbitrary. Other Skeletest modules
should not use any of these identifiers directly.
-}
module Skeletest.Internal.Constants (
  mainFileSpecsListIdentifier,
  mainFileTestSrcsIdentifier,
) where

import Data.Text

-- | The name of the list of Specs collected from test modules
-- in the Main module.
mainFileSpecsListIdentifier :: Text
mainFileSpecsListIdentifier = "skeletest_all_specs"

-- | The name of the TestSrcs definition in the Main module.
mainFileTestSrcsIdentifier :: Text
mainFileTestSrcsIdentifier = "skeletest_test_srcs"
