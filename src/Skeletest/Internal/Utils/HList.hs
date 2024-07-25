{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.Utils.HList (
  HList (..),
  uncheck,
  toListWith,
  toListWithM,
  hzip,
  hzipWithM,
) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (runIdentity)
import GHC.Generics ((:*:) (..))

data HList f xs where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f (x ': xs)

uncheck :: HList (Const a) xs -> [a]
uncheck = toListWith getConst

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

hzipWithM ::
  (Monad m) =>
  (forall x. f x -> g x -> m (h x))
  -> HList f xs
  -> HList g xs
  -> m (HList h xs)
hzipWithM k = \cases
  HNil HNil -> pure HNil
  (HCons f fs) (HCons g gs) -> HCons <$> k f g <*> hzipWithM k fs gs
