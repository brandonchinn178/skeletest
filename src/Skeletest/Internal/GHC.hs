{-# LANGUAGE LambdaCase #-}
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
  HsType (..),
  HsName,
) where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC (
  GenLocated (..),
  GhcPs,
  unLoc,
 )
import GHC qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Types.Name qualified as GHC.Name

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
  GHC.FunBind{fun_id} -> [ModuleValFun $ hsNameFromLIdP $ unLoc fun_id]
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
      GHC.IEName _ name -> Just $ ModuleExportVar $ hsNameFromLIdP $ unLoc name
      GHC.IEPattern _ name -> Just $ ModuleExportPattern $ hsNameFromLIdP $ unLoc name
      GHC.IEType _ _ -> Nothing

{----- HsExpr -----}

data HsExpr
  = HsExprVar HsName
  | HsExprApp HsExpr [HsExpr]
  | HsExprList [HsExpr]
  | HsExprTuple [HsExpr]
  | HsExprRecordCon HsName [(HsName, HsExpr)]

fromHsExpr :: HsExpr -> GHC.LHsExpr GhcPs
fromHsExpr = \case
  HsExprVar name -> genLoc $ GHC.HsVar GHC.noExtField (genLoc $ toRdrName GHC.Name.varName name)
  HsExprApp expr0 exprs ->
    foldl'
      (\acc -> genLoc . GHC.HsApp GHC.noExtField acc . fromHsExpr)
      (fromHsExpr expr0)
      exprs
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
        ( GHC.HsRecFields
            [ genLoc $
                GHC.HsFieldBind
                  GHC.noAnn
                  (genLoc $ GHC.FieldOcc GHC.noExtField (genLoc $ toRdrName (GHC.Name.fieldName $ fsText con) field))
                  (fromHsExpr expr)
                  False
            | (field, expr) <- fields
            ]
            Nothing
        )

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

-- TODO
type HsPat = ()

fromHsPat :: HsPat -> GHC.LPat GhcPs
fromHsPat _ = undefined

{----- HsName -----}

type HsName = Text

hsNameFromLIdP :: GHC.IdP GhcPs -> HsName
hsNameFromLIdP = Text.pack . GHC.occNameString . GHC.rdrNameOcc

toRdrName :: GHC.NameSpace -> HsName -> GHC.RdrName
toRdrName ns name =
  case Text.breakOnEnd "." name of
    (pre, name') | Just (modl, '.') <- Text.unsnoc pre -> GHC.mkQual ns (fsText modl, fsText name')
    _ -> GHC.mkUnqual ns (fsText name)

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
