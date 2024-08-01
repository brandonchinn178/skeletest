{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_10 (
  module Skeletest.Internal.GHC.Compat_9_10,
) where

import Data.Data (toConstr)
import GHC

import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
hsLamSingle = HsLam noAnn LamSingle

lamAltSingle :: HsMatchContext fn
lamAltSingle = LamAlt LamSingle

xCaseRn :: XCase GhcRn
xCaseRn = CaseAlt

hsLit :: HsLit (GhcPass p) -> HsExpr (GhcPass p)
hsLit = HsLit noExtField

hsPar :: forall p. (IsPass p) => LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsPar =
  HsPar $
    case ghcPass @p of
      GhcPs -> noAnn
      GhcRn -> noExtField
      GhcTc -> invariantViolation "hsPar called in GhcTc"

unHsPar :: HsExpr GhcRn -> LHsExpr GhcRn
unHsPar = \case
  HsPar _ e -> e
  e -> invariantViolation $ "unHsPar called on " <> (show . toConstr) e

hsTupPresent :: LHsExpr (GhcPass p) -> HsTupArg (GhcPass p)
hsTupPresent = Present noExtField

hsApp :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsApp = HsApp noExtField

genLoc :: (NoAnn ann) => e -> GenLocated (EpAnn ann) e
genLoc = L noAnn
