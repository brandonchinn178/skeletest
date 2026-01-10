module Skeletest.Internal.Spec.Tree (
  SpecTree (..),
) where

import Data.Text (Text)
import Skeletest.Internal.Markers (SomeMarker)
import Skeletest.Internal.TestRunner (TestResult)

data SpecTree
  = SpecGroup
      { groupLabel :: Text
      , groupTrees :: [SpecTree]
      }
  | SpecTest
      { testName :: Text
      , testMarkers :: [SomeMarker]
      -- ^ Markers, in order from least to most recently applied.
      --
      -- >>> withMarker MarkerA . withMarker MarkerB $ test ...
      --
      -- will contain
      --
      -- >>> SpecTest { testMarkers = [MarkerA, MarkerB] }
      , testAction :: IO TestResult
      }
