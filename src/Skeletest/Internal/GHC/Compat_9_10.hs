{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_10 (
  module Skeletest.Internal.GHC.Compat_9_10,
) where

import Data.Data (toConstr)
import GHC

import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
hsLamSingle = HsLam noAnn LamSingle

lamAltSingle :: HsMatchContext fn
lamAltSingle = LamAlt LamSingle

hsLit :: HsLit GhcPs -> HsExpr GhcPs
hsLit = HsLit noExtField

hsPar :: LHsExpr GhcPs -> HsExpr GhcPs
hsPar = HsPar noAnn

unHsPar :: HsExpr GhcPs -> LHsExpr GhcPs
unHsPar = \case
  HsPar _ e -> e
  e -> invariantViolation $ "unHsPar called on " <> (show . toConstr) e

hsTupPresent :: LHsExpr GhcPs -> HsTupArg GhcPs
hsTupPresent = Present noExtField

hsApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
hsApp = HsApp noExtField

genLoc :: (NoAnn ann) => e -> GenLocated (EpAnn ann) e
genLoc = L noAnn
