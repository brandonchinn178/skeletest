module Skeletest.Internal.Markers (
  IsMarker (..),
  SomeMarker (..),
  findMarker,
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

class (Show a, Typeable a) => IsMarker a where
  getMarkerName :: a -> Text

data SomeMarker = forall a. IsMarker a => SomeMarker a

deriving instance Show SomeMarker

-- | Find the first marker in the given list with the given type.
findMarker :: forall a. IsMarker a => [SomeMarker] -> Maybe a
findMarker = listToMaybe . mapMaybe (\(SomeMarker m) -> cast m)
