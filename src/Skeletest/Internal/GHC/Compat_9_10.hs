{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_10 (
  module Skeletest.Internal.GHC.Compat_9_10,
) where

import Data.Data (toConstr)
import GHC hiding (FieldOcc (..), mkPrefixFunRhs)
import GHC qualified
import GHC.Types.Name.Reader (getRdrName)
import Skeletest.Internal.Error (invariantViolation)

hsLamSingle :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> HsExpr (GhcPass p)
hsLamSingle = HsLam noAnn LamSingle

lamAltSingle :: HsMatchContext fn
lamAltSingle = LamAlt LamSingle

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
hsApp = HsApp noExtField

genLoc :: (NoAnn ann) => e -> GenLocated ann e
genLoc = L noAnn

mkPrefixFunRhs :: fn -> [ann] -> HsMatchContext fn
mkPrefixFunRhs fn _ = GHC.mkPrefixFunRhs fn

toMatchArgs :: [LPat p] -> [LPat p]
toMatchArgs = id
