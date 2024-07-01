{-# LANGUAGE LambdaCase #-}

module Skeletest.Assertions (
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
) where

import Skeletest.Internal.Predicate (Predicate, runPredicate)
import Skeletest.Internal.Predicate qualified as P

shouldBe :: Eq a => a -> a -> IO ()
actual `shouldBe` expected = actual `shouldSatisfy` P.eq expected

shouldNotBe :: Eq a => a -> a -> IO ()
actual `shouldNotBe` expected = actual `shouldNotSatisfy` P.eq expected

shouldSatisfy :: a -> Predicate a -> IO ()
actual `shouldSatisfy` p =
  runPredicate p actual >>= \case
    True -> pure ()
    False -> error "bad" -- TODO: fix

shouldNotSatisfy :: a -> Predicate a -> IO ()
actual `shouldNotSatisfy` p = actual `shouldSatisfy` P.not p
