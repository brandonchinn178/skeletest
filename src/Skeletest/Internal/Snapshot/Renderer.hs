{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Snapshot.Renderer (
  SnapshotRenderer (..),
  getSnapshotRenderers,
  setSnapshotRenderers,

  -- * Renderer implementations
  plainRenderer,
  renderWithShow,
  defaultSnapshotRenderers,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TextL
import Data.Text.Lazy.Encoding qualified as TextL
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

data SnapshotRenderer
  = forall a.
  (Typeable a) =>
  SnapshotRenderer
  { render :: a -> Text
  , snapshotLang :: Maybe Text
  }

plainRenderer :: (Typeable a) => (a -> Text) -> SnapshotRenderer
plainRenderer render =
  SnapshotRenderer
    { render
    , snapshotLang = Nothing
    }

renderWithShow :: forall a. (Typeable a, Show a) => SnapshotRenderer
renderWithShow = plainRenderer (Text.pack . show @a)

defaultSnapshotRenderers :: [SnapshotRenderer]
defaultSnapshotRenderers =
  [ plainRenderer @String Text.pack
  , plainRenderer @Text id
  , jsonRenderer
  ]
 where
  jsonRenderer =
    SnapshotRenderer
      { render = TextL.toStrict . TextL.decodeUtf8 . Aeson.encodePretty @Aeson.Value
      , snapshotLang = Just "json"
      }

snapshotRenderersRef :: IORef [SnapshotRenderer]
snapshotRenderersRef = unsafePerformIO $ newIORef []
{-# NOINLINE snapshotRenderersRef #-}

setSnapshotRenderers :: [SnapshotRenderer] -> IO ()
setSnapshotRenderers = writeIORef snapshotRenderersRef

getSnapshotRenderers :: (MonadIO m) => m [SnapshotRenderer]
getSnapshotRenderers = readIORef snapshotRenderersRef
