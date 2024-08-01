module Skeletest.Internal.Markers (
  IsMarker (..),
  AnonMarker (..),
  SomeMarker (..),
  findMarker,
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Typeable (Typeable, cast)

class (Show a, Typeable a) => IsMarker a where
  -- | The name of the marker that can be selected with '@name' syntax.
  --
  -- Marker names must only include alphanumeric characters, hyphens,
  -- underscores, and periods.
  getMarkerName :: a -> String

-- | A marker that can be used for bespoke marker definitions.
newtype AnonMarker = AnonMarker String
  deriving (Show)

instance IsMarker AnonMarker where
  getMarkerName (AnonMarker name) = name

data SomeMarker = forall a. (IsMarker a) => SomeMarker a

deriving instance Show SomeMarker

-- | Find the first marker in the given list with the given type.
findMarker :: forall a. (IsMarker a) => [SomeMarker] -> Maybe a
findMarker = listToMaybe . mapMaybe (\(SomeMarker m) -> cast m)
