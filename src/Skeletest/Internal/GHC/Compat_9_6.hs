{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_6 (
  module Skeletest.Internal.GHC.Compat_9_6,
) where

import Data.Data (toConstr)
import GHC
import GHC.Types.SrcLoc

import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
hsLamSingle = HsLam noExtField

lamAltSingle :: HsMatchContext fn
lamAltSingle = LambdaExpr

hsLit :: HsLit GhcPs -> HsExpr GhcPs
hsLit = HsLit noAnn

hsPar :: LHsExpr GhcPs -> HsExpr GhcPs
hsPar e = HsPar noAnn (L NoTokenLoc HsTok) e (L NoTokenLoc HsTok)

unHsPar :: HsExpr GhcPs -> LHsExpr GhcPs
unHsPar = \case
  HsPar _ _ e _ -> e
  e -> invariantViolation $ "unHsPar called on " <> (show . toConstr) e

hsTupPresent :: LHsExpr GhcPs -> HsTupArg GhcPs
hsTupPresent = Present noAnn

hsApp :: LHsExpr GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
hsApp = HsApp noAnn

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L (SrcSpanAnn noAnn generatedSrcSpan)
