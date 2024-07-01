{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}

module Skeletest.Internal.Predicate (
  Predicate,
  runPredicate,
  purePred,
  ioPred,

  -- * General
  eq,
  anything,

  -- * Data types
  just,
  -- TODO: nothing,
  -- TODO: tup2,
  -- TODO: tup3,
  -- TODO: tup4,

  -- * Numeric
  approx,
  tol,
  Tolerance (..),

  -- * Combinators
  not,
  -- TODO: P.any, P.all

  -- * IO
  returns,
  -- TODO: throws

  -- * Snapshot testing
  matchesSnapshot,
) where

import Data.Typeable (Typeable)
import Prelude hiding (abs, not)
import Prelude qualified

import Skeletest.Internal.Snapshot (
  defaultSnapshotRenderers,
  renderVal,
 )

newtype Predicate a = Predicate (a -> IO Bool)

runPredicate :: Predicate a -> a -> IO Bool
runPredicate (Predicate p) = p

purePred :: (a -> Bool) -> Predicate a
purePred f = Predicate $ pure . f

ioPred :: (a -> IO Bool) -> Predicate a
ioPred = Predicate

{----- General -----}

eq :: Eq a => a -> Predicate a
eq expected = purePred (== expected)

anything :: Predicate a
anything = purePred (const True)

{----- Data types -----}

just :: Predicate a -> Predicate (Maybe a)
just (Predicate p) = Predicate $ \case
  Just a -> p a
  Nothing -> pure False

{----- Numeric -----}

-- | A predicate for checking that a value is equal within some tolerance.
--
-- Useful for checking equality with floats, which might not be exactly equal.
-- For more information, see: https://jvns.ca/blog/2023/01/13/examples-of-floating-point-problems/.
--
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.abs = 1e-12} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6, P.abs = 1e-12} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Nothing} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Nothing, P.abs = 1e-12} 0.3
approx :: (Fractional a, Ord a) => Tolerance -> a -> Predicate a
approx Tolerance{..} expected = Predicate $ \actual -> pure $ Prelude.abs (actual - expected) <= tolerance
  where
    mRelTol = fromTol <$> rel
    absTol = fromTol abs
    tolerance =
      case mRelTol of
        Just relTol -> max (relTol * Prelude.abs expected) absTol
        Nothing -> absTol

    fromTol x
      | x < 0 = error $ "tolerance can't be negative: " <> show x
      | otherwise = fromRational x

data Tolerance = Tolerance
  { rel :: Maybe Rational
  , abs :: Rational
  }

tol :: Tolerance
tol = Tolerance{rel = Just 1e-6, abs = 1e-12}

{----- Combinators -----}

not :: Predicate a -> Predicate a
not (Predicate p) = Predicate (fmap Prelude.not . p)

{----- IO -----}

returns :: Predicate a -> Predicate (IO a)
returns (Predicate p) = ioPred (p =<<)

{----- Snapshot -----}

-- TODO: --update to update snapshots
-- TODO: if no filters were added, error if outdated snapshot files (--update to remove)
-- TODO: if all tests in file were run, error if snapshot file contains outdated tests (--update to remove)
matchesSnapshot :: Typeable a => Predicate a
matchesSnapshot = Predicate $ \a -> do
  -- TODO: check snapshot file
  _ <- pure $ renderVal (customRenderers <> defaultSnapshotRenderers) a
  pure True
  where
    -- TODO: load from SkeletestOptions
    customRenderers = []
