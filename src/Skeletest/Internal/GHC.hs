{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| Provide a pure API for GHC internals.

All GHC operations should go through this API, to isolate
the rest of the logic from GHC internals logic, which can
include breaking changes between versions.
-}
module Skeletest.Internal.GHC (
  Plugin,
  PluginDef (..),
  mkPlugin,

  -- * ParsedModule
  ParsedModule,
  getModuleVals,
  modifyModuleExprs,
  FunDef (..),
  addModuleFun,

  -- ** Module values
  ModuleVal (..),
  moduleValName,

  -- ** Expressions
  HsExpr (..),
  hsApps,
  collectApps,
  renderHsExpr,

  -- ** Types
  HsType (..),

  -- ** Patterns
  HsPat (..),

  -- ** Names
  HsName,
  hsName,
  hsNewName,
  getHsName,
  getHsNameMod,
  renderHsName,
) where

import Data.Data (Data)
import Data.Data qualified as Data
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import GHC (
  GenLocated (..),
  GhcPs,
  unLoc,
 )
import GHC qualified as GHC
import GHC.Driver.Main qualified as GHC
import GHC.Plugins qualified as GHC hiding (getHscEnv)
import GHC.Types.Name qualified as GHC.Name
import GHC.Types.Name.Cache qualified as GHC (NameCache)
import GHC.Types.SourceText qualified as GHC.SourceText
import Language.Haskell.TH.Syntax qualified as TH
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

import Skeletest.Internal.Error (skeletestPluginError)
import Skeletest.Internal.GHC.Compat (genLoc)
import Skeletest.Internal.GHC.Compat qualified as GHC.Compat

-- Has to be exactly GHC's Plugin type, for GHC to register it correctly.
type Plugin = GHC.Plugin

-- | Our pure definition of PluginDef, agnostic of GHC version.
data PluginDef = PluginDef
  { isPure :: Bool
  , afterParse ::
      Text -- Module name
      -> ParsedModule -- Parsed module
      -> ParsedModule -- Post-processed module
  }

mkPlugin :: PluginDef -> Plugin
mkPlugin PluginDef{..} =
  GHC.defaultPlugin
    { GHC.pluginRecompile = if isPure then GHC.purePlugin else GHC.impurePlugin
    , GHC.parsedResultAction = \_ modInfo result -> do
        env <- GHC.getHscEnv
        let
          GHC.Module{moduleName} = GHC.ms_mod modInfo
          moduleNameT = Text.pack $ GHC.moduleNameString moduleName
          mkParsedModule modl =
            ParsedModule
              { ghcModule = modl
              , fromHsName = unsafePerformIO . hsNameToRdrName (GHC.hsc_NC env)
              }
        pure
          $ (ghcModule . afterParse moduleNameT . mkParsedModule)
            & modifyHpmModule
            & modifyParsedResultModule
          $ result
    }
  where
    modifyParsedResultModule f x = x{GHC.parsedResultModule = f $ GHC.parsedResultModule x}
    modifyHpmModule f x = x{GHC.hpm_module = f $ GHC.hpm_module x}

{----- ParsedModule -----}

data ParsedModule = ParsedModule
  { ghcModule :: GHC.Located (GHC.HsModule GhcPs)
  , fromHsName :: HsName -> GHC.RdrName
  }

-- | Get the list of values defined in the module.
getModuleVals :: ParsedModule -> [ModuleVal]
getModuleVals = concatMap (fromHsBind . unLoc) . GHC.hsmodDecls . unLoc . ghcModule
  where
    fromHsBind = \case
      GHC.ValD _ bind -> toModuleVals bind
      _ -> []

modifyModuleExprs :: (HsExpr -> Maybe HsExpr) -> ParsedModule -> ParsedModule
modifyModuleExprs f parsedModule = parsedModule{ghcModule = go <$> ghcModule parsedModule}
  where
    ParsedModule{fromHsName} = parsedModule

    go :: (Data a) => a -> a
    go = Data.gmapT $ \(x :: a) ->
      go $
        case Typeable.eqT @(GHC.LHsExpr GhcPs) @a of
          Just Typeable.Refl | Just expr <- f $ toHsExpr x -> fromHsExpr fromHsName expr
          _ -> x

data FunDef = FunDef
  { funName :: Text
  , funType :: HsType
  , funPats :: [HsPat]
  , funBody :: HsExpr
  }

addModuleFun :: FunDef -> ParsedModule -> ParsedModule
addModuleFun FunDef{..} parsedModule = parsedModule{ghcModule = update <$> ghcModule parsedModule}
  where
    ParsedModule{fromHsName} = parsedModule

    update modl = modl{GHC.hsmodDecls = decls <> GHC.hsmodDecls modl}
    name = genLoc $ GHC.mkUnqual GHC.Name.varName $ fsText funName
    decls =
      [ mkSigD name (fromHsType fromHsName funType)
      , genLoc . GHC.ValD GHC.noExtField $
          GHC.FunBind GHC.noExtField name . GHC.MG GHC.FromSource . genLoc $
            [ genLoc $
                GHC.Match
                  GHC.noAnn
                  (GHC.FunRhs name GHC.Prefix GHC.NoSrcStrict)
                  (map (fromHsPat fromHsName) funPats)
                  ( GHC.GRHSs
                      GHC.emptyComments
                      [genLoc $ GHC.GRHS GHC.noAnn [] (fromHsExpr fromHsName funBody)]
                      (GHC.EmptyLocalBinds GHC.noExtField)
                  )
            ]
      ]

{----- ModuleVal -----}

data ModuleVal
  = ModuleValFun HsName
  deriving (Show, Eq)

moduleValName :: ModuleVal -> HsName
moduleValName = \case
  ModuleValFun name -> name

toModuleVals :: GHC.HsBind GhcPs -> [ModuleVal]
toModuleVals = \case
  GHC.FunBind{fun_id} -> [ModuleValFun $ hsRdrName $ unLoc fun_id]
  _ -> []

{----- HsExpr -----}

data HsExpr
  = HsExprCon HsName
  | HsExprVar HsName
  | HsExprApp HsExpr HsExpr
  | HsExprOp HsExpr HsExpr HsExpr -- lhs op rhs
  | HsExprList [HsExpr]
  | HsExprTuple [HsExpr]
  | HsExprRecordCon HsName [(HsName, HsExpr)]
  | HsExprLitString Text
  | HsExprLam HsPat HsExpr
  | HsExprCase HsExpr [(HsPat, HsExpr)]
  | HsExprOther (WithShow (GHC.LHsExpr GhcPs))
  deriving (Show)

hsApps :: HsExpr -> [HsExpr] -> HsExpr
hsApps = foldl' HsExprApp

-- | Collect an application of the form `((f a) b) c` and return `f [a, b, c]`.
collectApps :: HsExpr -> (HsExpr, [HsExpr])
collectApps = \case
  HsExprApp l r -> let (f, xs) = collectApps l in (f, xs <> [r])
  e -> (e, [])

toHsExpr :: GHC.LHsExpr GhcPs -> HsExpr
toHsExpr = \case
  L _ (GHC.HsVar _ (L _ name)) ->
    if GHC.occNameSpace (GHC.rdrNameOcc name) == GHC.Name.dataName
      then HsExprCon (hsRdrName name)
      else HsExprVar (hsRdrName name)
  L _ (GHC.HsApp _ lhs rhs) -> HsExprApp (toHsExpr lhs) (toHsExpr rhs)
  L _ (GHC.OpApp _ lhs op rhs) -> HsExprOp (toHsExpr lhs) (toHsExpr op) (toHsExpr rhs)
  L _ (GHC.ExplicitList _ lexprs) -> HsExprList $ map toHsExpr lexprs
  L _ (GHC.ExplicitTuple _ args _)
    | Just presentArgs <- mapM getPresentTupArg args ->
        HsExprTuple $ map toHsExpr presentArgs
  L _ (GHC.RecordCon _ conName GHC.HsRecFields{rec_flds}) ->
    let
      getField GHC.HsFieldBind{hfbLHS = field, hfbRHS = expr} =
        ( hsRdrName . unLoc . GHC.foLabel . unLoc $ field
        , toHsExpr expr
        )
     in
      HsExprRecordCon (hsRdrName $ unLoc conName) $ map (getField . unLoc) rec_flds
  L _ (GHC.HsLit _ (GHC.HsString _ s)) -> HsExprLitString $ Text.pack $ GHC.unpackFS s
  L _ par@GHC.HsPar{} | let expr = GHC.Compat.unHsPar par -> toHsExpr expr
  expr -> HsExprOther $ WithShow expr
  where
    getPresentTupArg = \case
      GHC.Present _ lexpr -> Just lexpr
      _ -> Nothing

fromHsExpr :: (HsName -> GHC.RdrName) -> HsExpr -> GHC.LHsExpr GhcPs
fromHsExpr toRdrName = go
  where
    go = \case
      HsExprCon name -> genLoc $ GHC.HsVar GHC.noExtField (genLoc $ toRdrName name)
      HsExprVar name -> genLoc $ GHC.HsVar GHC.noExtField (genLoc $ toRdrName name)
      HsExprApp l r -> genLoc $ GHC.Compat.hsApp (parens $ go l) (parens $ go r)
      HsExprOp l op r -> genLoc $ GHC.OpApp GHC.noAnn (parens $ go l) (parens $ go op) (parens $ go r)
      HsExprList exprs -> genLoc $ GHC.ExplicitList GHC.noAnn $ map go exprs
      HsExprTuple exprs ->
        genLoc $
          GHC.ExplicitTuple
            GHC.noAnn
            (map (GHC.Compat.hsTupPresent . go) exprs)
            GHC.Boxed
      HsExprRecordCon con fields ->
        genLoc $
          GHC.RecordCon
            GHC.noAnn
            (genLoc $ toRdrName con)
            GHC.HsRecFields
              { rec_flds =
                  [ genLoc $
                      GHC.HsFieldBind
                        { hfbAnn = GHC.noAnn
                        , hfbLHS = genLoc $ GHC.FieldOcc GHC.noExtField (genLoc $ toRdrName field)
                        , hfbRHS = go expr
                        , hfbPun = False
                        }
                  | (field, expr) <- fields
                  ]
              , rec_dotdot = Nothing
              }
      HsExprLitString s -> genLoc $ GHC.Compat.hsLit $ GHC.HsString GHC.SourceText.NoSourceText (fsText s)
      HsExprLam pat expr ->
        genLoc . GHC.Compat.hsLamSingle $
          GHC.MG GHC.FromSource . genLoc $
            [ genLoc $
                GHC.Match
                  { m_ext = GHC.noAnn
                  , m_ctxt = GHC.Compat.lamAltSingle
                  , m_pats = [fromHsPat toRdrName pat]
                  , m_grhss =
                      GHC.GRHSs
                        { grhssExt = GHC.emptyComments
                        , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] $ go expr]
                        , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                        }
                  }
            ]
      HsExprCase expr matches ->
        genLoc . GHC.HsCase GHC.noAnn (go expr) $
          GHC.MG GHC.FromSource . genLoc $
            [ genLoc $
                GHC.Match
                  { m_ext = GHC.noAnn
                  , m_ctxt = GHC.CaseAlt
                  , m_pats = [fromHsPat toRdrName pat]
                  , m_grhss =
                      GHC.GRHSs
                        { grhssExt = GHC.emptyComments
                        , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] $ go body]
                        , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                        }
                  }
            | (pat, body) <- matches
            ]
      HsExprOther (WithShow expr) -> expr

    parens = \case
      e@(L _ GHC.HsApp{}) -> genLoc $ GHC.Compat.hsPar e
      e@(L _ GHC.SectionL{}) -> genLoc $ GHC.Compat.hsPar e
      e@(L _ GHC.SectionR{}) -> genLoc $ GHC.Compat.hsPar e
      e -> e

renderHsExpr :: ParsedModule -> HsExpr -> Text
renderHsExpr parsedModule = Text.pack . show . WithShow . fromHsExpr fromHsName
  where
    ParsedModule{fromHsName} = parsedModule

{----- HsType -----}

data HsType
  = HsTypeCon HsName
  | HsTypeApp HsType [HsType]
  | HsTypeTuple [HsType]

fromHsType :: (HsName -> GHC.RdrName) -> HsType -> GHC.LHsType GhcPs
fromHsType fromHsName = go
  where
    go = \case
      HsTypeCon name -> genLoc $ GHC.HsTyVar GHC.noAnn GHC.NotPromoted (genLoc $ fromHsName name)
      HsTypeApp ty0 tys ->
        foldl'
          (\acc -> genLoc . GHC.HsAppTy GHC.noExtField acc . go)
          (go ty0)
          tys
      HsTypeTuple tys -> genLoc $ GHC.HsTupleTy GHC.noAnn GHC.HsBoxedOrConstraintTuple $ map go tys

{----- HsPat -----}

data HsPat
  = HsPatCon HsName [HsPat]
  | HsPatVar HsName
  | HsPatRecord HsName [(HsName, HsPat)]
  | HsPatWild
  deriving (Show)

fromHsPat :: (HsName -> GHC.RdrName) -> HsPat -> GHC.LPat GhcPs
fromHsPat fromHsName = go
  where
    go = \case
      HsPatCon conName args ->
        genLoc $
          GHC.ConPat
            GHC.noAnn
            (genLoc $ fromHsName conName)
            (GHC.PrefixCon [] $ map go args)
      HsPatVar name -> genLoc $ GHC.VarPat GHC.noExtField (genLoc $ fromHsName name)
      HsPatRecord conName fields ->
        genLoc $
          GHC.ConPat
            GHC.noAnn
            (genLoc $ fromHsName conName)
            ( GHC.RecCon
                GHC.HsRecFields
                  { rec_flds =
                      [ genLoc $
                          GHC.HsFieldBind
                            { hfbAnn = GHC.noAnn
                            , hfbLHS = genLoc $ GHC.FieldOcc GHC.noExtField (genLoc $ fromHsName field)
                            , hfbRHS = go pat
                            , hfbPun = False
                            }
                      | (field, pat) <- fields
                      ]
                  , rec_dotdot = Nothing
                  }
            )
      HsPatWild -> genLoc $ GHC.WildPat GHC.noExtField

{----- HsName -----}

data HsName
  = HsName TH.Name
  | HsNewName Text
  | HsRdrName (WithShow GHC.RdrName)
  deriving (Show, Eq)

hsName :: TH.Name -> HsName
hsName = HsName

hsNewName :: Text -> HsName
hsNewName = HsNewName

hsRdrName :: GHC.RdrName -> HsName
hsRdrName = HsRdrName . WithShow

hsNameToRdrName :: GHC.NameCache -> HsName -> IO GHC.RdrName
hsNameToRdrName nc = \case
  HsName name ->
    GHC.thNameToGhcNameIO nc name >>= \case
      Just n -> pure $ GHC.getRdrName n
      Nothing -> skeletestPluginError $ "Could not get Name for `" <> show name <> "`"
  HsNewName name -> pure $ GHC.mkUnqual GHC.Name.varName (fsText name)
  HsRdrName (WithShow name) -> pure name

getHsName :: HsName -> Text
getHsName = \case
  HsName name -> Text.pack . TH.nameBase $ name
  HsNewName name -> name
  HsRdrName (WithShow name) -> Text.pack . GHC.occNameString . GHC.rdrNameOcc $ name

getHsNameMod :: HsName -> Maybe Text
getHsNameMod = \case
  HsName name -> Text.pack <$> TH.nameModule name
  HsNewName _ -> Nothing
  HsRdrName (WithShow name) ->
    case name of
      GHC.Qual modl _ -> Just . Text.pack . GHC.moduleNameString $ modl
      _ -> Nothing

renderHsName :: HsName -> Text
renderHsName = \case
  HsName name -> Text.pack . show $ name
  HsNewName name -> name
  HsRdrName name -> Text.pack . show $ name

{----- GHC.HsDecl -----}

mkSigD :: GHC.LIdP GhcPs -> GHC.LHsType GhcPs -> GHC.LHsDecl GhcPs
mkSigD name ty =
  genLoc . GHC.SigD GHC.noExtField $
    GHC.TypeSig GHC.noAnn [name] $
      GHC.HsWC GHC.noExtField (genLoc $ GHC.HsSig GHC.noExtField (GHC.HsOuterImplicit GHC.noExtField) ty)

{----- FastString -----}

fsText :: Text -> GHC.FastString
fsText = GHC.fsLit . Text.unpack

{----- Utilities -----}

newtype WithShow a = WithShow a
  deriving (Eq)

instance (GHC.Outputable a) => Show (WithShow a) where
  show (WithShow a) = GHC.showSDocUnsafe $ GHC.ppr a
