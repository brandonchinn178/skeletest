{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_8 (
  module Skeletest.Internal.GHC.Compat_9_8,
) where

import Data.Data (toConstr)
import GHC hiding (FieldOcc (..), mkPrefixFunRhs)
import GHC qualified
import GHC.Types.Name.Reader (getRdrName)
import GHC.Types.SrcLoc

import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
hsLamSingle = HsLam noExtField

lamAltSingle :: HsMatchContext fn
lamAltSingle = LambdaExpr

hsLit :: HsLit (GhcPass p) -> HsExpr (GhcPass p)
hsLit = HsLit noAnn

hsPar :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsPar e = HsPar noAnn (L NoTokenLoc HsTok) e (L NoTokenLoc HsTok)

unHsPar :: HsExpr GhcRn -> LHsExpr GhcRn
unHsPar = \case
  HsPar _ _ e _ -> e
  e -> invariantViolation $ "unHsPar called on " <> (show . toConstr) e

hsTupPresent :: LHsExpr (GhcPass p) -> HsTupArg (GhcPass p)
hsTupPresent = Present noAnn

xMatch :: XCMatch (GhcPass p) b
xMatch = noAnn

mkHsRecFields :: [LHsRecField (GhcPass p) arg] -> HsRecFields (GhcPass p) arg
mkHsRecFields fields =
  GHC.HsRecFields
    { rec_flds = fields
    , rec_dotdot = Nothing
    }

foLabel :: GHC.FieldOcc GhcRn -> LIdP GhcRn
foLabel = genLoc . GHC.foExt

fieldOccRn :: Name -> GHC.FieldOcc GhcRn
fieldOccRn name =
  GHC.FieldOcc
    { GHC.foExt = name
    , GHC.foLabel = genLoc $ getRdrName name
    }

hsApp :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsApp = HsApp noAnn

genLoc :: e -> GenLocated (SrcAnn ann) e
genLoc = L (SrcSpanAnn noAnn generatedSrcSpan)

mkPrefixFunRhs :: LIdP GhcPs -> EpAnn () -> HsMatchContext GhcPs
mkPrefixFunRhs fn _ = GHC.mkPrefixFunRhs fn

toMatchArgs :: [LPat p] -> [LPat p]
toMatchArgs = id
