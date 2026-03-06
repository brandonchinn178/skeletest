{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Error (
  SkeletestError (..),
  skeletestError,
  skeletestPluginError,
  invariantViolation,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC qualified
import UnliftIO.Exception (Exception (..), impureThrow, throwIO)

data SkeletestError
  = -- | Thrown for most errors, unless an error needs to be specially caught.
    SkeletestError Text
  | -- | A user error during compilation, e.g. during the preprocessor or plugin phases.
    CompilationError (Maybe GHC.SrcSpan) Text
  deriving (Show)

instance Exception SkeletestError where
  displayException =
    Text.unpack . \case
      SkeletestError msg -> msg
      CompilationError _ msg ->
        Text.unlines
          [ ""
          , "******************** skeletest failure ********************"
          , msg
          ]

skeletestError :: (MonadIO m) => Text -> m a
skeletestError = throwIO . SkeletestError

skeletestPluginError :: Maybe GHC.SrcSpan -> String -> a
skeletestPluginError mloc = impureThrow . CompilationError mloc . Text.pack

invariantViolation :: String -> a
invariantViolation = impureThrow . SkeletestError . Text.pack . toMessage
 where
  toMessage msg =
    unlines
      [ "Invariant violation: " <> msg
      , "**** This is a skeletest bug. Please report it at https://github.com/brandonchinn178/skeletest/issues"
      ]
