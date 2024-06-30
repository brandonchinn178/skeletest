module Skeletest.Internal.Predicate (
  Predicate (..),
  eq,
  returns,
) where

newtype Predicate a = Predicate (a -> IO Bool)

eq :: Eq a => a -> Predicate a
eq expected = Predicate $ \actual -> pure $ actual == expected

returns :: Eq a => a -> Predicate (IO a)
returns expected = Predicate $ \m -> (== expected) <$> m
