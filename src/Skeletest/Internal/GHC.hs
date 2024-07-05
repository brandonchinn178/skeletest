{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| Provide a pure API for GHC internals.

All GHC operations should go through this API, to isolate
the rest of the logic from GHC internals logic, which can
include breaking changes between versions.

TODO: support 9.6 + 9.8
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
  getModuleExports,
  addModuleExport,

  -- ** Module values
  ModuleVal (..),
  moduleValName,

  -- ** Module exports
  ModuleExports (..),
  ModuleExport (..),
  ModuleExportTypeContents (..),
  moduleExportName,

  -- ** Expressions
  HsExpr (..),
  hsApps,
  collectApps,

  -- ** Types
  HsType (..),

  -- ** Patterns
  HsPat (..),

  -- ** Names
  HsName,
  hsName,
  hsQualName,
  getHsName,
) where

import Data.Data (Data)
import Data.Data qualified as Data
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import GHC (
  GenLocated (..),
  GhcPs,
  unLoc,
 )
import GHC qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Types.Name qualified as GHC.Name
import GHC.Types.SourceText qualified as GHC.SourceText

-- Has to be exactly GHC's Plugin type, for GHC to register it correctly.
type Plugin = GHC.Plugin

-- | Our pure definition of PluginDef, agnostic of GHC version.
data PluginDef = PluginDef
  { isPure :: Bool
  , afterParse ::
      Text -- ^ Module name
      -> ParsedModule -- ^ Parsed module
      -> ParsedModule -- ^ Post-processed module
  }

mkPlugin :: PluginDef -> Plugin
mkPlugin PluginDef{..} =
  GHC.defaultPlugin
    { GHC.pluginRecompile = if isPure then GHC.purePlugin else GHC.impurePlugin
    , GHC.parsedResultAction = \_ modInfo result -> do
        let
          GHC.Module{moduleName} = GHC.ms_mod modInfo
          name = Text.pack $ GHC.moduleNameString moduleName
        pure $
          (unParsedModule . afterParse name . ParsedModule)
            & modifyHpmModule
            & modifyParsedResultModule
            $ result
    }
  where
    modifyParsedResultModule f x = x{GHC.parsedResultModule = f $ GHC.parsedResultModule x}
    modifyHpmModule f x = x{GHC.hpm_module = f $ GHC.hpm_module x}

{----- ParsedModule -----}

newtype ParsedModule = ParsedModule {unParsedModule :: GHC.Located (GHC.HsModule GhcPs)}

-- | Get the list of values defined in the module.
getModuleVals :: ParsedModule -> [ModuleVal]
getModuleVals = concatMap (fromHsBind . unLoc) . GHC.hsmodDecls . unLoc . unParsedModule
  where
    fromHsBind = \case
      GHC.ValD _ bind -> toModuleVals bind
      _ -> []

modifyModuleExprs :: (HsExpr -> Maybe HsExpr) -> ParsedModule -> ParsedModule
modifyModuleExprs f = ParsedModule . go . unParsedModule
  where
    go :: Data a => a -> a
    go = Data.gmapT $ \(x :: a) ->
      go $
        case Typeable.eqT @(GHC.LHsExpr GhcPs) @a of
          Just Typeable.Refl | Just expr <- f $ toHsExpr x -> fromHsExpr expr
          _ -> x

getModuleExports :: ParsedModule -> ModuleExports
getModuleExports = toModuleExports . GHC.hsmodExports . unLoc . unParsedModule

data FunDef = FunDef
  { funName :: HsName
  , funType :: HsType
  , funPats :: [HsPat]
  , funBody :: HsExpr
  }

addModuleFun :: FunDef -> ParsedModule -> ParsedModule
addModuleFun FunDef{..} = ParsedModule . fmap update . unParsedModule
  where
    update modl = modl{GHC.hsmodDecls = decls <> GHC.hsmodDecls modl}
    name = genLoc $ toRdrName GHC.Name.varName funName
    decls =
      [ mkSigD name (fromHsType funType)
      , genLoc . GHC.ValD GHC.noExtField $
          GHC.FunBind GHC.noExtField name . GHC.MG GHC.FromSource . genLoc $
            [ genLoc $
                GHC.Match
                  GHC.noAnn
                  (GHC.FunRhs name GHC.Prefix GHC.NoSrcStrict)
                  (map fromHsPat funPats)
                  ( GHC.GRHSs
                      GHC.emptyComments
                      [genLoc $ GHC.GRHS GHC.noAnn [] (fromHsExpr funBody)]
                      (GHC.EmptyLocalBinds GHC.noExtField)
                  )
            ]
      ]

addModuleExport :: ModuleExport -> ParsedModule -> ParsedModule
addModuleExport _ = id -- error "TODO: addModuleExport"

{----- ModuleVal -----}

data ModuleVal
  = ModuleValFun HsName
  deriving (Show, Eq)

moduleValName :: ModuleVal -> HsName
moduleValName = \case
  ModuleValFun name -> name

toModuleVals :: GHC.HsBind GhcPs -> [ModuleVal]
toModuleVals = \case
  GHC.FunBind{fun_id} -> [ModuleValFun $ hsGhcName $ unLoc fun_id]
  _ -> []

{----- ModuleExport -----}

data ModuleExports
  = ModuleExportEverything
  | ModuleExportList [ModuleExport]
  deriving (Show, Eq)

data ModuleExport
  = ModuleExportVar HsName
  | ModuleExportPattern HsName
  | ModuleExportType HsName ModuleExportTypeContents
  deriving (Show, Eq)

data ModuleExportTypeContents
  = ModuleExportTypeNoContents
    -- ^ An export item without any contents, either because it can't (type),
    -- or it's not exporting anything (typeclass)
  | ModuleExportTypeEverything
    -- ^ An export item exporting everything
  | ModuleExportTypeList [HsName]
    -- ^ An export item exporting the given names
  deriving (Show, Eq)

moduleExportName :: ModuleExport -> HsName
moduleExportName = \case
  ModuleExportVar name -> name
  ModuleExportPattern name -> name
  ModuleExportType name _ -> name

toModuleExports :: Maybe (GHC.LocatedL [GHC.LIE GhcPs]) -> ModuleExports
toModuleExports = \case
  Nothing -> ModuleExportEverything
  Just (L _ exports) -> ModuleExportList $ mapMaybe (fromIE . unLoc) exports
  where
    fromIE = \case
      GHC.IEVar _ wrappedName _ -> fromIEWrappedName $ unLoc wrappedName
      GHC.IEThingAbs _ wrappedName _ -> do
        name <- unwrapName wrappedName
        pure $ ModuleExportType name ModuleExportTypeNoContents
      GHC.IEThingAll _ wrappedName _ -> do
        name <- unwrapName wrappedName
        pure $ ModuleExportType name ModuleExportTypeEverything
      GHC.IEThingWith _ wrappedName _ contents _ -> do
        name <- unwrapName wrappedName
        pure . ModuleExportType name . ModuleExportTypeList $
          mapMaybe unwrapName contents
      _ -> Nothing

    unwrapName :: GHC.LIEWrappedName GhcPs -> Maybe HsName
    unwrapName = fmap moduleExportName . fromIEWrappedName . unLoc

    fromIEWrappedName :: GHC.IEWrappedName GhcPs -> Maybe ModuleExport
    fromIEWrappedName = \case
      GHC.IEName _ name -> Just $ ModuleExportVar $ hsGhcName $ unLoc name
      GHC.IEPattern _ name -> Just $ ModuleExportPattern $ hsGhcName $ unLoc name
      GHC.IEType _ _ -> Nothing

{----- HsExpr -----}

data HsExpr
  = HsExprCon HsName
  | HsExprVar HsName
  | HsExprApp HsExpr HsExpr
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
      then HsExprCon (hsGhcName name)
      else HsExprVar (hsGhcName name)
  L _ (GHC.HsApp _ lhs rhs) -> HsExprApp (toHsExpr lhs) (toHsExpr rhs)
  L _ (GHC.ExplicitList _ lexprs) -> HsExprList $ map toHsExpr lexprs
  L _ (GHC.ExplicitTuple _ args _) | Just presentArgs <- mapM getPresentTupArg args ->
    HsExprTuple $ map toHsExpr presentArgs
  L _ (GHC.RecordCon _ conName GHC.HsRecFields{rec_flds}) ->
    let
      getField GHC.HsFieldBind{hfbLHS = field, hfbRHS = expr} =
        ( hsGhcName . unLoc . GHC.foLabel . unLoc $ field
        , toHsExpr expr
        )
    in
      HsExprRecordCon (hsGhcName $ unLoc conName) $ map (getField . unLoc) rec_flds
  L _ (GHC.HsLit _ (GHC.HsString _ s)) -> HsExprLitString $ Text.pack $ GHC.unpackFS s
  L _ (GHC.HsPar _ expr) -> toHsExpr expr
  expr -> HsExprOther $ WithShow expr
  where
    getPresentTupArg = \case
      GHC.Present _ lexpr -> Just lexpr
      _ -> Nothing

fromHsExpr :: HsExpr -> GHC.LHsExpr GhcPs
fromHsExpr = \case
  HsExprCon name -> genLoc $ GHC.HsVar GHC.noExtField (genLoc $ toRdrName GHC.Name.dataName name)
  HsExprVar name -> genLoc $ GHC.HsVar GHC.noExtField (genLoc $ toRdrName GHC.Name.varName name)
  HsExprApp l r -> genLoc $ GHC.HsApp GHC.noExtField (parens $ fromHsExpr l) (parens $ fromHsExpr r)
  HsExprList exprs -> genLoc $ GHC.ExplicitList GHC.noAnn $ map fromHsExpr exprs
  HsExprTuple exprs ->
    genLoc $
      GHC.ExplicitTuple
        GHC.noAnn
        (map (GHC.Present GHC.noExtField . fromHsExpr) exprs)
        GHC.Boxed
  HsExprRecordCon con fields ->
    genLoc $
      GHC.RecordCon
        GHC.noAnn
        (genLoc $ toRdrName GHC.Name.dataName con)
        GHC.HsRecFields
          { rec_flds =
              [ genLoc $
                  GHC.HsFieldBind
                    { hfbAnn = GHC.noAnn
                    , hfbLHS = genLoc $ GHC.FieldOcc GHC.noExtField (genLoc $ toRdrName (GHC.Name.fieldName $ fsText $ getHsName con) field)
                    , hfbRHS = fromHsExpr expr
                    , hfbPun = False
                    }
              | (field, expr) <- fields
              ]
          , rec_dotdot = Nothing
          }
  HsExprLitString s -> genLoc $ GHC.HsLit GHC.noExtField $ GHC.HsString GHC.SourceText.NoSourceText (fsText s)
  HsExprLam pat expr ->
    genLoc . GHC.HsLam GHC.noAnn GHC.LamSingle $
      GHC.MG GHC.FromSource . genLoc $
        [ genLoc $
            GHC.Match
              { m_ext = GHC.noAnn
              , m_ctxt = GHC.LamAlt GHC.LamSingle
              , m_pats = [fromHsPat pat]
              , m_grhss =
                  GHC.GRHSs
                    { grhssExt = GHC.emptyComments
                    , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] $ fromHsExpr expr]
                    , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                    }
              }
        ]
  HsExprCase expr matches ->
    genLoc . GHC.HsCase GHC.noAnn (fromHsExpr expr) $
      GHC.MG GHC.FromSource . genLoc $
        [ genLoc $
            GHC.Match
              { m_ext = GHC.noAnn
              , m_ctxt = GHC.CaseAlt
              , m_pats = [fromHsPat pat]
              , m_grhss =
                  GHC.GRHSs
                    { grhssExt = GHC.emptyComments
                    , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] $ fromHsExpr body]
                    , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                    }
              }
        | (pat, body) <- matches
        ]
  HsExprOther (WithShow expr) -> expr
  where
    parens = \case
      e@(L _ (GHC.HsApp _ _ _)) -> genLoc $ GHC.HsPar (GHC.NoEpTok, GHC.NoEpTok) e
      e -> e

{----- HsType -----}

data HsType
  = HsTypeCon HsName
  | HsTypeApp HsType [HsType]
  | HsTypeTuple [HsType]

fromHsType :: HsType -> GHC.LHsType GhcPs
fromHsType = \case
  HsTypeCon name -> genLoc $ GHC.HsTyVar [] GHC.NotPromoted (genLoc $ toRdrName GHC.Name.tcName name)
  HsTypeApp ty0 tys ->
    foldl'
      (\acc -> genLoc . GHC.HsAppTy GHC.noExtField acc . fromHsType)
      (fromHsType ty0)
      tys
  HsTypeTuple tys -> genLoc $ GHC.HsTupleTy GHC.noAnn GHC.HsBoxedOrConstraintTuple $ map fromHsType tys

{----- HsPat -----}

data HsPat
  = HsPatCon HsName [HsPat]
  | HsPatVar HsName
  | HsPatRecord HsName [(HsName, HsPat)]
  | HsPatWild
  deriving (Show)

fromHsPat :: HsPat -> GHC.LPat GhcPs
fromHsPat = \case
  HsPatCon conName args ->
    genLoc $
      GHC.ConPat
        GHC.noAnn
        (genLoc $ toRdrName GHC.Name.dataName conName)
        (GHC.PrefixCon [] $ map fromHsPat args)
  HsPatVar name -> genLoc $ GHC.VarPat GHC.noExtField (genLoc $ toRdrName GHC.Name.varName name)
  HsPatRecord conName fields ->
    genLoc $
      GHC.ConPat
        GHC.noAnn
        (genLoc $ toRdrName GHC.Name.dataName conName)
        ( GHC.RecCon
            GHC.HsRecFields
              { rec_flds =
                  [ genLoc $
                      GHC.HsFieldBind
                        { hfbAnn = GHC.noAnn
                        , hfbLHS = genLoc $ GHC.FieldOcc GHC.noExtField (genLoc $ toRdrName (GHC.Name.fieldName $ fsText $ getHsName conName) field)
                        , hfbRHS = fromHsPat pat
                        , hfbPun = False
                        }
                  | (field, pat) <- fields
                  ]
              , rec_dotdot = Nothing
              }
        )
  HsPatWild -> genLoc $ GHC.WildPat GHC.noExtField

{----- HsName -----}

data HsName = HsNewName (Maybe Text) Text | HsGhcName (WithShow GHC.RdrName)
  deriving (Show, Eq)

hsName :: Text -> HsName
hsName = HsNewName Nothing

hsQualName :: Text -> Text -> HsName
hsQualName = HsNewName . Just

hsGhcName :: GHC.RdrName -> HsName
hsGhcName = HsGhcName . WithShow

getHsName :: HsName -> Text
getHsName = \case
  HsNewName mQual name -> maybe "" (<> ".") mQual <> name
  HsGhcName (WithShow name) -> Text.pack . GHC.occNameString . GHC.rdrNameOcc $ name

toRdrName :: GHC.NameSpace -> HsName -> GHC.RdrName
toRdrName ns = \case
  HsNewName Nothing name -> GHC.mkUnqual ns (fsText name)
  HsNewName (Just qual) name -> GHC.mkQual ns (fsText qual, fsText name)
  HsGhcName (WithShow name) -> name

{----- GHC.HsDecl -----}

mkSigD :: GHC.LIdP GhcPs -> GHC.LHsType GhcPs -> GHC.LHsDecl GhcPs
mkSigD name ty =
  genLoc . GHC.SigD GHC.noExtField $ GHC.TypeSig GHC.noAnn [name] $
    GHC.HsWC GHC.noExtField (genLoc $ GHC.HsSig GHC.noExtField (GHC.HsOuterImplicit GHC.noExtField) ty)

{----- Locations -----}

genLoc :: (GHC.NoAnn ann) => e -> GenLocated (GHC.EpAnn ann) e
genLoc = L GHC.noAnn

{----- FastString -----}

fsText :: Text -> GHC.FastString
fsText = GHC.fsLit . Text.unpack

{----- Utilities -----}

newtype WithShow a = WithShow a
  deriving (Eq)

instance GHC.Outputable a => Show (WithShow a) where
  show (WithShow a) = GHC.showSDocUnsafe $ GHC.ppr a
