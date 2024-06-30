{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Spec (
  -- * Spec interface
  Spec,
  SpecTree (..),
  getSpecTrees,
  filterSpec,
  runSpec,

  -- ** Defining a Spec
  describe,
  it,
  prop,
) where

import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import UnliftIO.Exception (SomeException, displayException, trySyncOrAsync)

import Skeletest.Prop.Internal (Property, runProperty)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

data SpecTree
  = SpecGroup Text [SpecTree]
  | SpecTest Text (IO ())

-- | Filter specs bottom-to-top.
filterSpec :: (SpecTree -> Bool) -> Spec -> Spec
filterSpec f = Spec . tell . go . getSpecTrees
  where
    go :: [SpecTree] -> [SpecTree]
    go = filter f . map recurseGroups

    recurseGroups = \case
      SpecGroup name trees -> SpecGroup name (go trees)
      SpecTest name io -> SpecTest name io

runSpec :: Spec -> IO ()
runSpec = go 0 . getSpecTrees
  where
    -- TODO: colors
    go !lvl = mapM_ $ \case
      SpecGroup name trees -> do
        Text.putStrLn $ indent lvl name
        go (lvl + 1) trees
      SpecTest name io -> do
        Text.putStr $ indent lvl (name <> ": ")
        trySyncOrAsync io >>= \case
          Right () -> do
            Text.putStrLn "OK"
          Left (e :: SomeException) -> do
            -- TODO: catch Skeletest failure, show FAIL
            Text.putStrLn "ERROR"
            Text.putStrLn $ indent lvl (Text.pack $ displayException e)

    indent lvl = Text.intercalate "\n" . map (Text.replicate (lvl * 4) " " <>) . Text.splitOn "\n"

{----- Defining a Spec -----}

describe :: String -> Spec -> Spec
describe name spec = Spec $ tell [SpecGroup (Text.pack name) (getSpecTrees spec)]

it :: String -> IO () -> Spec
it name action = Spec $ tell [SpecTest (Text.pack name) action]

prop :: String -> Property -> Spec
prop name p = Spec $ tell [SpecTest (Text.pack name) (runProperty p)]
