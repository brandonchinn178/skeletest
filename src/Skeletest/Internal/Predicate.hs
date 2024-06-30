{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}

module Skeletest.Internal.Predicate (
  Predicate (..),
  eq,
  approx,
  returns,

  -- * Tolerance
  tol,
  Tolerance (..),
) where

import Prelude hiding (abs)
import Prelude qualified

newtype Predicate a = Predicate (a -> IO Bool)

eq :: Eq a => a -> Predicate a
eq expected = Predicate $ pure . (== expected)

returns :: Predicate a -> Predicate (IO a)
returns (Predicate p) = Predicate (p =<<)

{----- approx -----}

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
