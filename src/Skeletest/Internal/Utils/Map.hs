module Skeletest.Internal.Utils.Map (
  findOrEmpty,
  adjustNested,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Exts (IsList, fromList)

findOrEmpty :: (Ord k, IsList (t a)) => k -> Map k (t a) -> t a
findOrEmpty = Map.findWithDefault (fromList [])

-- | Same as 'adjust', except defaulting to an empty structure if it doesn't
-- exist, and deleting the key if the adjusted value is empty.
adjustNested :: (Ord k, Foldable t, IsList (t a)) => (t a -> t a) -> k -> Map k (t a) -> Map k (t a)
adjustNested f =
  let prune m = if null m then Nothing else Just m
   in Map.alter (prune . f . fromMaybe (fromList []))
