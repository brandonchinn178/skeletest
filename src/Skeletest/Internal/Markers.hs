module Skeletest.Internal.Markers (
  IsMarker (..),
  AnonMarker (..),
  SomeMarker (..),
  findMarker,
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

class (Show a, Typeable a) => IsMarker a where
  -- | The name of the marker that can be selected with '@name' syntax.
  getMarkerName :: a -> Text

  -- | If true, skips tests if no selections are specified on the command
  -- line. Defaults to false.
  isManualMarker :: a -> Bool
  isManualMarker _ = False

-- | A marker that can be used for bespoke marker definitions.
data AnonMarker = AnonMarker
  { anonMarkerName :: Text
  , anonMarkerManual :: Bool
  }
  deriving (Show)

instance IsMarker AnonMarker where
  getMarkerName = anonMarkerName
  isManualMarker = anonMarkerManual

data SomeMarker = forall a. IsMarker a => SomeMarker a

deriving instance Show SomeMarker

-- | Find the first marker in the given list with the given type.
findMarker :: forall a. IsMarker a => [SomeMarker] -> Maybe a
findMarker = listToMaybe . mapMaybe (\(SomeMarker m) -> cast m)
