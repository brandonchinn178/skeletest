{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.Utils.HList (
  HList (..),
  toListWith,
  toListWithM,
  hzip,
) where

import Data.Functor.Identity (runIdentity)
import GHC.Generics ((:*:) (..))

data HList f xs where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

toListWith :: (forall x. f x -> y) -> HList f xs -> [y]
toListWith f = runIdentity . toListWithM (pure . f)

toListWithM :: (Monad m) => (forall x. f x -> m y) -> HList f xs -> m [y]
toListWithM f = \case
  HNil -> pure []
  HCons x xs -> (:) <$> f x <*> toListWithM f xs

hzip :: HList f xs -> HList g xs -> HList (f :*: g) xs
hzip = \cases
  HNil HNil -> HNil
  (HCons f fs) (HCons g gs) -> HCons (f :*: g) (hzip fs gs)
