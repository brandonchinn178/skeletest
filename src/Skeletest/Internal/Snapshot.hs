module Skeletest.Internal.Snapshot (
  SnapshotRenderer (..),
  defaultSnapshotRenderers,
  renderVal,
) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Debug.RecoverRTTI (anythingToString)

-- TODO: Skeletest.Internal.Snapshot
data SnapshotRenderer =
  forall a.
  Typeable a =>
  SnapshotRenderer
    { render :: a -> Text
    }

defaultSnapshotRenderers :: [SnapshotRenderer]
defaultSnapshotRenderers =
  [ SnapshotRenderer @String Text.pack
  , SnapshotRenderer @Text id
  ]

renderVal :: Typeable a => [SnapshotRenderer] -> a -> Text
renderVal renderers a =
  case mapMaybe tryRender renderers of
    [] -> Text.pack $ anythingToString a
    rendered : _ -> rendered
  where
    tryRender SnapshotRenderer{render} = render <$> Typeable.cast a
