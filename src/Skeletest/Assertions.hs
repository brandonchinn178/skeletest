{-# LANGUAGE LambdaCase #-}

module Skeletest.Assertions (
  shouldBe,
  shouldSatisfy,
) where

import Skeletest.Internal.Predicate (Predicate, runPredicate)
import Skeletest.Internal.Predicate qualified as P

shouldBe :: Eq a => a -> a -> IO ()
actual `shouldBe` expected = actual `shouldSatisfy` P.eq expected

shouldSatisfy :: a -> Predicate a -> IO ()
actual `shouldSatisfy` p =
  runPredicate p actual >>= \case
    True -> pure ()
    False -> error "bad" -- TODO: fix
