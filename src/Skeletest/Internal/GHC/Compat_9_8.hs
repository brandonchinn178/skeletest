{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_8 (
  module Skeletest.Internal.GHC.Compat_9_8,
) where

import Data.Data (toConstr)
import GHC
import GHC.Types.SrcLoc

import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
hsLamSingle = HsLam noExtField

lamAltSingle :: HsMatchContext fn
lamAltSingle = LambdaExpr

hsLit :: HsLit (GhcPass p) -> HsExpr (GhcPass p)
hsLit = HsLit noAnn

hsPar :: LHsExpr p -> HsExpr p
hsPar e = HsPar noAnn (L NoTokenLoc HsTok) e (L NoTokenLoc HsTok)

unHsPar :: HsExpr GhcRn -> LHsExpr GhcRn
unHsPar = \case
  HsPar _ _ e _ -> e
  e -> invariantViolation $ "unHsPar called on " <> (show . toConstr) e

hsTupPresent :: LHsExpr (GhcPass p) -> HsTupArg (GhcPass p)
hsTupPresent = Present noAnn

hsApp :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsApp = HsApp noAnn

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L (SrcSpanAnn noAnn generatedSrcSpan)
