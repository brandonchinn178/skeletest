{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Skeletest.Internal.GHC.Compat_9_12 (
  module Skeletest.Internal.GHC.Compat_9_12,
  mkPrefixFunRhs,
) where

import Data.Data (toConstr)
import GHC hiding (FieldOcc (..))
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
xMatch = noExtField

mkHsRecFields ::
  forall p arg.
  (IsPass p) =>
  [LHsRecField (GhcPass p) arg]
  -> HsRecFields (GhcPass p) arg
mkHsRecFields fields =
  GHC.HsRecFields
    { rec_ext =
        case ghcPass @p of
          GhcPs -> noExtField
          GhcRn -> noExtField
          GhcTc -> invariantViolation "mkHsRecFields called in GhcTc"
    , rec_flds = fields
    , rec_dotdot = Nothing
    }

foLabel :: GHC.FieldOcc GhcRn -> LIdP GhcRn
foLabel = GHC.foLabel

fieldOccRn :: Name -> GHC.FieldOcc GhcRn
fieldOccRn name =
  GHC.FieldOcc
    { GHC.foExt = getRdrName name
    , GHC.foLabel = genLoc name
    }

hsApp :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
hsApp = HsApp noExtField

genLoc :: (NoAnn ann) => e -> GenLocated ann e
genLoc = L noAnn

toMatchArgs :: [LPat p] -> LocatedE [LPat p]
toMatchArgs = genLoc
