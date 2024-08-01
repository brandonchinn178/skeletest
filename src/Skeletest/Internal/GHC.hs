{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
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
  Ctx (..),
  GhcRn,
  mkPlugin,

  -- * ParsedModule
  ParsedModule (..),
  FunDef (..),

  -- ** Expressions
  HsExpr,
  HsExprData (..),
  hsExprCon,
  hsExprVar,
  hsExprApps,
  hsExprList,
  hsExprRecordCon,
  hsExprLitString,
  hsExprLam,
  hsExprCase,
  getExpr,
  renderHsExpr,

  -- ** Types
  HsType (..),

  -- ** Patterns
  HsPat (..),

  -- ** Names
  HsName,
  hsName,
  hsVarName,
  getHsName,
) where

import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.State qualified as State
import Data.Data (Data)
import Data.Data qualified as Data
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable qualified as Typeable
import GHC (
  GenLocated (..),
  GhcPass (..),
  GhcPs,
  GhcRn,
  IsPass,
  unLoc,
 )
import GHC qualified as GHC
import GHC.Driver.Main qualified as GHC
import GHC.Plugins qualified as GHC hiding (getHscEnv)
import GHC.Tc.Utils.Monad qualified as GHC
import GHC.Types.Name qualified as GHC.Name
import GHC.Types.Name.Cache qualified as GHC (NameCache)
import GHC.Types.SourceText qualified as GHC.SourceText
import Language.Haskell.TH.Syntax qualified as TH
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

import Skeletest.Internal.Error (invariantViolation, skeletestPluginError)
import Skeletest.Internal.GHC.Compat (genLoc)
import Skeletest.Internal.GHC.Compat qualified as GHC.Compat

-- Has to be exactly GHC's Plugin type, for GHC to register it correctly.
type Plugin = GHC.Plugin

-- | Our pure definition of PluginDef, agnostic of GHC version.
data PluginDef = PluginDef
  { isPure :: Bool
  , modifyParsed :: ModuleName -> ParsedModule -> ParsedModule
  , onRename :: Ctx -> ModuleName -> HsExpr GhcRn -> HsExpr GhcRn
  }

data Ctx = Ctx
  { matchesName :: HsName GhcRn -> HsName GhcRn -> Bool
  }

type ModuleName = Text

mkPlugin :: PluginDef -> Plugin
mkPlugin PluginDef{..} =
  GHC.defaultPlugin
    { GHC.pluginRecompile = if isPure then GHC.purePlugin else GHC.impurePlugin
    , GHC.parsedResultAction = \_ modInfo result -> do
        let
          moduleName = getModuleName $ GHC.ms_mod modInfo
          parsedModule = initParsedModule . GHC.hpm_module . GHC.parsedResultModule $ result
          ParsedModule{moduleFuncs} = modifyParsed moduleName parsedModule
        newDecls <-
          runCompilePs . fmap concat . sequence $
            [ compileFunDef funName funDef
            | (funName, Just funDef) <- moduleFuncs
            ]
        pure
          . (modifyParsedResultModule . modifyHpmModule . fmap . modifyModDecls) (newDecls <>)
          $ result
    , GHC.renamedResultAction = \_ gblEnv group -> do
        nameCache <- GHC.hsc_NC . GHC.env_top <$> GHC.getEnv
        let
          moduleName = getModuleName $ GHC.tcg_mod gblEnv
          ctx =
            Ctx
              { matchesName = matchesNameImpl nameCache
              }
        group' <- runCompileRn $ modifyModuleExprs (onRename ctx moduleName) group
        pure (gblEnv, group')
    }
  where
    getModuleName GHC.Module{moduleName} = Text.pack $ GHC.moduleNameString moduleName

    modifyParsedResultModule f x = x{GHC.parsedResultModule = f $ GHC.parsedResultModule x}
    modifyHpmModule f x = x{GHC.hpm_module = f $ GHC.hpm_module x}
    modifyModDecls f x = x{GHC.hsmodDecls = f $ GHC.hsmodDecls x}

{----- ParsedModule -----}

data ParsedModule = ParsedModule
  { moduleFuncs :: [(HsName GhcPs, Maybe FunDef)]
  }

data FunDef = FunDef
  { funType :: HsType GhcPs
  , funPats :: [HsPat GhcPs]
  , funBody :: HsExpr GhcPs
  }

initParsedModule :: GHC.Located (GHC.HsModule GhcPs) -> ParsedModule
initParsedModule (L _ GHC.HsModule{hsmodDecls}) =
  ParsedModule
    { moduleFuncs =
        [ (funName, Nothing)
        | Just funName <- map (getValName . unLoc) hsmodDecls
        ]
    }
  where
    getValName = \case
      GHC.ValD _ GHC.FunBind{fun_id} -> Just . hsGhcName . unLoc $ fun_id
      _ -> Nothing

{----- modifyModuleExprs -----}

modifyModuleExprs ::
  forall m.
  (MonadCompile m GhcRn) =>
  (HsExpr GhcRn -> HsExpr GhcRn)
  -> GHC.HsGroup GhcRn
  -> m (GHC.HsGroup GhcRn)
modifyModuleExprs f = go
  where
    go :: (Data a) => a -> m a
    go = Data.gmapM $ \x -> updateExpr x >>= go

    updateExpr :: (Data a) => a -> m a
    updateExpr (x :: a) =
      case Typeable.eqT @(GHC.LHsExpr GhcRn) @a of
        Just Typeable.Refl -> compileHsExpr . f . parseHsExpr $ x
        Nothing -> pure x

{----- HsExpr -----}

-- | A Haskell expression that is either:
--     1. A parsed expression from the compiler
--          * ghcExpr is Just
--          * hsExpr is not HsExprOther if the expression is something
--            we care about parsing, otherwise HsExprOther
--     3. A new expression we're creating
--          * ghcExpr is Nothing
--          * hsExpr is not HsExprOther
--
-- Invariants:
--   * If ghcExpr is Just, hsExpr must not have been modified
--   * if ghcExpr is Nothing, hsExpr is not HsExprOther
data HsExpr p = HsExprUnsafe
  { ghcExpr :: Maybe (GhcLHsExpr p)
  , hsExpr :: HsExprData p
  }
  deriving (Show)

data HsExprData p
  = HsExprCon (HsName p)
  | HsExprVar (HsName p)
  | HsExprApps (HsExpr p) [HsExpr p]
  | HsExprOp (HsExpr p) (HsExpr p) (HsExpr p) -- lhs op rhs
  | HsExprList [HsExpr p]
  | HsExprRecordCon (HsName p) [(HsName p, HsExpr p)]
  | HsExprLitString Text
  | HsExprLam [HsPat p] (HsExpr p)
  | HsExprCase (HsExpr p) [(HsPat p, HsExpr p)]
  | HsExprOther
  deriving (Show)

getExpr :: HsExpr p -> HsExprData p
getExpr HsExprUnsafe{hsExpr} = hsExpr

renderHsExpr :: HsExpr GhcRn -> Text
renderHsExpr = \case
  HsExprUnsafe{ghcExpr = Just e} -> Text.pack $ show e
  HsExprUnsafe{hsExpr = e} -> Text.pack $ show e

newHsExpr :: HsExprData p -> HsExpr p
newHsExpr e =
  HsExprUnsafe
    { ghcExpr = Nothing
    , hsExpr = e
    }

hsExprCon :: HsName p -> HsExpr p
hsExprCon = newHsExpr . HsExprCon

hsExprVar :: HsName p -> HsExpr p
hsExprVar = newHsExpr . HsExprVar

hsExprApps :: HsExpr p -> [HsExpr p] -> HsExpr p
hsExprApps f xs = newHsExpr $ HsExprApps f xs

hsExprList :: [HsExpr p] -> HsExpr p
hsExprList = newHsExpr . HsExprList

hsExprRecordCon :: HsName p -> [(HsName p, HsExpr p)] -> HsExpr p
hsExprRecordCon conName fields = newHsExpr $ HsExprRecordCon conName fields

hsExprLitString :: Text -> HsExpr p
hsExprLitString = newHsExpr . HsExprLitString

hsExprLam :: [HsPat p] -> HsExpr p -> HsExpr p
hsExprLam args body = newHsExpr $ HsExprLam args body

hsExprCase :: HsExpr p -> [(HsPat p, HsExpr p)] -> HsExpr p
hsExprCase e branches = newHsExpr $ HsExprCase e branches

parseHsExpr :: GHC.LHsExpr GhcRn -> HsExpr GhcRn
parseHsExpr = goExpr
  where
    goExpr e =
      HsExprUnsafe
        { ghcExpr = Just $ GhcLHsExprRn e
        , hsExpr = goData e
        }

    goData = \case
      L _ (GHC.HsVar _ (L _ name)) ->
        if (GHC.occNameSpace . GHC.occName) name == GHC.Name.dataName
          then HsExprCon (hsGhcName name)
          else HsExprVar (hsGhcName name)
      e@(L _ GHC.HsApp{}) ->
        let (f, xs) = collectApps e
         in HsExprApps (goExpr f) (map goExpr xs)
      L _ (GHC.OpApp _ lhs op rhs) ->
        HsExprOp (goExpr lhs) (goExpr op) (goExpr rhs)
      L _ (GHC.RecordCon _ conName GHC.HsRecFields{rec_flds}) ->
        HsExprRecordCon (hsGhcName $ unLoc conName) $ map (getRecField . unLoc) rec_flds
      L _ par@GHC.HsPar{} -> goData $ GHC.Compat.unHsPar par
      _ -> HsExprOther

    getRecField GHC.HsFieldBind{hfbLHS = field, hfbRHS = expr} =
      (hsGhcName . GHC.foExt . unLoc $ field, goExpr expr)

    -- Collect an application of the form `((f a) b) c` and return `f [a, b, c]`
    collectApps = \case
      L _ (GHC.HsApp _ l r) -> let (f, xs) = collectApps l in (f, xs <> [r])
      e -> (e, [])

{----- HsType -----}

data HsType p
  = HsTypeCon (HsName p)
  | HsTypeApps (HsType p) [HsType p]
  | HsTypeTuple [HsType p]

{----- HsPat -----}

data HsPat p
  = HsPatCon (HsName p) [HsPat p]
  | HsPatVar (HsName p)
  | HsPatRecord (HsName p) [(HsName p, HsPat p)]
  | HsPatWild
  deriving (Show)

{----- HsName -----}

data HsName p
  = HsName TH.Name
  | HsVarName Text
  | HsGhcName (GhcIdP p)
  deriving (Show, Eq)

hsName :: TH.Name -> HsName p
hsName = HsName

hsVarName :: Text -> HsName p
hsVarName = HsVarName

hsGhcName :: forall p. (IsPass p) => GHC.IdP (GhcPass p) -> HsName (GhcPass p)
hsGhcName = HsGhcName . onPsOrRn @p GhcIdPs GhcIdRn

fromTHName :: GHC.NameCache -> TH.Name -> GHC.Name
fromTHName nameCache name =
  case unsafePerformIO $ GHC.thNameToGhcNameIO nameCache name of
    Just n -> n
    Nothing -> skeletestPluginError $ "Could not get Name for `" <> show name <> "`"

matchesNameImpl :: GHC.NameCache -> HsName GhcRn -> HsName GhcRn -> Bool
matchesNameImpl nameCache n1 n2 = fromMaybe False $ (==) <$> go n1 <*> go n2
  where
    go = \case
      HsName name -> Just $ fromTHName nameCache name
      HsVarName _ -> Nothing -- new names will never match
      HsGhcName name -> Just $ unGhcIdP name

getHsName :: HsName p -> Text
getHsName = \case
  HsName name -> Text.pack . TH.nameBase $ name
  HsVarName name -> name
  HsGhcName (GhcIdPs name) -> Text.pack . GHC.occNameString . GHC.rdrNameOcc $ name
  HsGhcName (GhcIdRn name) -> Text.pack . GHC.occNameString . GHC.nameOccName $ name

{----- Compilation -----}

class (Monad m) => MonadHasNameCache m where
  getNameCache :: m GHC.NameCache
class (Monad m) => MonadCompileName m p where
  mkIdP :: Text -> m (GHC.IdP p)

type MonadCompile m p = (MonadHasNameCache m, MonadCompileName m p)

newtype CompilePs a = CompilePs (GHC.Hsc a)
  deriving (Functor, Applicative, Monad)

runCompilePs :: CompilePs a -> GHC.Hsc a
runCompilePs (CompilePs m) = m

instance MonadHasNameCache CompilePs where
  getNameCache = GHC.hsc_NC <$> CompilePs GHC.getHscEnv
instance MonadCompileName CompilePs GhcPs where
  mkIdP = pure . GHC.mkUnqual GHC.Name.varName . fsText

newtype CompileRn a = CompileRn (StateT (Map Text GHC.Name) GHC.TcM a)
  deriving (Functor, Applicative, Monad)

runCompileRn :: CompileRn a -> GHC.TcM a
runCompileRn (CompileRn m) = evalStateT m Map.empty

instance MonadHasNameCache CompileRn where
  getNameCache = GHC.hsc_NC . GHC.env_top <$> (CompileRn . Trans.lift) GHC.getEnv
instance MonadCompileName CompileRn GhcRn where
  mkIdP name = do
    nameMap <- CompileRn State.get
    case Map.lookup name nameMap of
      Just name' -> pure name'
      Nothing -> do
        uniq <- (CompileRn . Trans.lift) GHC.getUniqueM
        let name' = GHC.mkSystemVarName uniq (fsText name)
        CompileRn $ State.put (Map.insert name name' nameMap)
        pure name'

compileHsName ::
  forall p m.
  (GHC.IsPass p, MonadCompile m (GhcPass p)) =>
  HsName (GhcPass p)
  -> m (GHC.IdP (GhcPass p))
compileHsName = \case
  HsName name -> do
    nameCache <- getNameCache
    pure . onPsOrRn @p GHC.getRdrName id $ fromTHName nameCache name
  HsVarName name -> mkIdP @_ @(GhcPass p) name
  HsGhcName name -> pure $ unGhcIdP name

compileFunDef :: (MonadCompile m GhcPs) => HsName GhcPs -> FunDef -> m [GHC.LHsDecl GhcPs]
compileFunDef funName FunDef{..} = do
  name <- compileHsName funName
  ty <- compileHsType funType
  pats <- mapM compileHsPat funPats
  body <- compileHsExpr funBody
  pure
    [ mkSigD name ty
    , genLoc . GHC.ValD GHC.noExtField $
        GHC.FunBind GHC.noExtField (genLoc name) . GHC.MG GHC.FromSource . genLoc $
          [ genLoc $
              GHC.Match
                GHC.noAnn
                (GHC.FunRhs (genLoc name) GHC.Prefix GHC.NoSrcStrict)
                pats
                ( GHC.GRHSs
                    GHC.emptyComments
                    [genLoc $ GHC.GRHS GHC.noAnn [] body]
                    (GHC.EmptyLocalBinds GHC.noExtField)
                )
          ]
    ]
  where
    mkSigD name ty =
      genLoc
        . GHC.SigD GHC.noExtField
        . GHC.TypeSig GHC.noAnn [genLoc name]
        . GHC.HsWC GHC.noExtField
        . genLoc
        $ GHC.HsSig GHC.noExtField (GHC.HsOuterImplicit GHC.noExtField) ty

compileHsType :: (MonadCompile m GhcPs) => HsType GhcPs -> m (GHC.LHsType GhcPs)
compileHsType = go
  where
    go = \case
      HsTypeCon name -> do
        genLoc . GHC.HsTyVar GHC.noAnn GHC.NotPromoted . genLoc <$> compileHsName name
      HsTypeApps ty0 tys -> do
        ty0' <- go ty0
        tys' <- mapM go tys
        pure $ foldl' (\l r -> genLoc $ GHC.HsAppTy GHC.noExtField l r) ty0' tys'
      HsTypeTuple tys -> do
        tys' <- mapM go tys
        pure . genLoc $ GHC.HsTupleTy GHC.noAnn GHC.HsBoxedOrConstraintTuple tys'

compileHsPat ::
  forall p m.
  (IsPass p, MonadCompile m (GhcPass p)) =>
  HsPat (GhcPass p)
  -> m (GHC.LPat (GhcPass p))
compileHsPat = go
  where
    go = \case
      HsPatCon conName args -> do
        conName' <- fromConName conName
        con <- GHC.PrefixCon [] <$> mapM go args
        pure . genLoc $
          GHC.ConPat
            (onPsOrRn @p GHC.noAnn GHC.noExtField)
            conName'
            con
      HsPatVar name -> do
        name' <- onPsOrRn @p genLoc genLoc <$> compileHsName name
        pure . genLoc $ GHC.VarPat GHC.noExtField name'
      HsPatRecord conName fields -> do
        conName' <- fromConName conName
        con <- GHC.RecCon <$> compileRecFields go fields
        pure . genLoc $
          GHC.ConPat
            (onPsOrRn @p GHC.noAnn GHC.noExtField)
            conName'
            con
      HsPatWild -> do
        pure . genLoc $ GHC.WildPat $ onPsOrRn @p GHC.noExtField GHC.noExtField

    fromConName = fmap (onPsOrRn @p genLoc genLoc) . compileHsName

compileHsExpr ::
  forall p m.
  (IsPass p, MonadCompile m (GhcPass p)) =>
  HsExpr (GhcPass p)
  -> m (GHC.LHsExpr (GhcPass p))
compileHsExpr = goExpr
  where
    goExpr :: HsExpr (GhcPass p) -> m (GHC.LHsExpr (GhcPass p))
    goExpr = \case
      HsExprUnsafe{ghcExpr = Just e} -> pure $ unGhcLHsExpr e
      HsExprUnsafe{hsExpr = e} -> goData e

    goData :: HsExprData (GhcPass p) -> m (GHC.LHsExpr (GhcPass p))
    goData = \case
      HsExprCon name -> do
        genLoc . GHC.HsVar GHC.noExtField . genLocIdP @p <$> compileHsName name
      HsExprVar name -> do
        genLoc . GHC.HsVar GHC.noExtField . genLocIdP @p <$> compileHsName name
      HsExprApps f xs -> do
        f' <- goExpr f
        xs' <- mapM goExpr xs
        pure $ foldl' (\l r -> genLoc $ GHC.Compat.hsApp l r) (parens f') (map parens xs')
      HsExprOp _ _ _ ->
        invariantViolation "Compiling HsExprOp not yet supported"
      HsExprList exprs -> do
        exprs' <- mapM goExpr exprs
        pure . genLoc $
          GHC.ExplicitList
            (onPsOrRn @p GHC.noAnn GHC.noExtField)
            exprs'
      HsExprRecordCon con fields -> do
        con' <- genLocConLikeP @p <$> compileHsName con
        fields' <- compileRecFields goExpr fields
        pure . genLoc $
          GHC.RecordCon
            (onPsOrRn @p GHC.noAnn GHC.noExtField)
            con'
            fields'
      HsExprLitString s -> do
        pure . genLoc . GHC.Compat.hsLit $
          GHC.HsString GHC.SourceText.NoSourceText (fsText s)
      HsExprLam pats expr -> do
        pats' <- mapM compileHsPat pats
        expr' <- goExpr expr
        pure . genLoc . GHC.Compat.hsLamSingle $
          GHC.MG origin . genLoc $
            [ genLoc $
                GHC.Match
                  { m_ext = GHC.noAnn
                  , m_ctxt = GHC.Compat.lamAltSingle
                  , m_pats = pats'
                  , m_grhss =
                      GHC.GRHSs
                        { grhssExt = GHC.emptyComments
                        , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] expr']
                        , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                        }
                  }
            ]
      HsExprCase expr matches -> do
        expr' <- goExpr expr
        matches' <-
          sequence
            [ do
                pat' <- compileHsPat pat
                body' <- goExpr body
                pure . genLoc $
                  GHC.Match
                    { m_ext = GHC.noAnn
                    , m_ctxt = GHC.CaseAlt
                    , m_pats = [pat']
                    , m_grhss =
                        GHC.GRHSs
                          { grhssExt = GHC.emptyComments
                          , grhssGRHSs = [genLoc $ GHC.GRHS GHC.noAnn [] body']
                          , grhssLocalBinds = GHC.EmptyLocalBinds GHC.noExtField
                          }
                    }
            | (pat, body) <- matches
            ]
        pure
          . genLoc
          . GHC.HsCase (onPsOrRn @p GHC.noAnn GHC.Compat.xCaseRn) expr'
          $ GHC.MG origin (genLoc matches')
      HsExprOther ->
        invariantViolation "Compiling HsExprOther not supported"

    origin = onPsOrRn @p GHC.FromSource GHC.FromSource

    parens :: (IsPass p) => GHC.LHsExpr (GhcPass p) -> GHC.LHsExpr (GhcPass p)
    parens = \case
      e@(L _ GHC.HsPar{}) -> e
      e@(L _ GHC.HsApp{}) -> genLoc $ GHC.Compat.hsPar e
      e@(L _ GHC.SectionL{}) -> genLoc $ GHC.Compat.hsPar e
      e@(L _ GHC.SectionR{}) -> genLoc $ GHC.Compat.hsPar e
      e -> e

{----- FastString -----}

fsText :: Text -> GHC.FastString
fsText = GHC.fsLit . Text.unpack

{----- Utilities -----}

data GhcIdP p where
  GhcIdPs :: GHC.RdrName -> GhcIdP GhcPs
  GhcIdRn :: GHC.Name -> GhcIdP GhcRn

instance Show (GhcIdP p) where
  show = \case
    GhcIdPs name -> renderOutputable name
    GhcIdRn name -> renderOutputable name

instance Eq (GhcIdP p) where
  GhcIdPs n1 == GhcIdPs n2 = n1 == n2
  GhcIdRn n1 == GhcIdRn n2 = n1 == n2

unGhcIdP :: GhcIdP p -> GHC.IdP p
unGhcIdP = \case
  GhcIdPs n -> n
  GhcIdRn n -> n

data GhcLHsExpr p where
  GhcLHsExprPs :: GHC.LHsExpr GhcPs -> GhcLHsExpr GhcPs
  GhcLHsExprRn :: GHC.LHsExpr GhcRn -> GhcLHsExpr GhcRn

instance Show (GhcLHsExpr p) where
  show = \case
    GhcLHsExprPs e -> renderOutputable e
    GhcLHsExprRn e -> renderOutputable e

unGhcLHsExpr :: GhcLHsExpr p -> GHC.LHsExpr p
unGhcLHsExpr = \case
  GhcLHsExprPs e -> e
  GhcLHsExprRn e -> e

newtype GhcFixity = GhcFixity GHC.Fixity

instance Show GhcFixity where
  show (GhcFixity fixity) = renderOutputable fixity

renderOutputable :: (GHC.Outputable a) => a -> String
renderOutputable = GHC.showSDocUnsafe . GHC.ppr

onPsOrRn :: forall p a. (IsPass p) => ((p ~ 'GHC.Parsed) => a) -> ((p ~ 'GHC.Renamed) => a) -> a
onPsOrRn ps rn =
  case GHC.ghcPass @p of
    GhcPs -> ps
    GhcRn -> rn
    GhcTc -> invariantViolation "onPsOrRn found GhcTc"

compileRecFields ::
  forall p m arg x.
  (IsPass p, MonadCompile m (GhcPass p)) =>
  (x -> m arg)
  -> [(HsName (GhcPass p), x)]
  -> m (GHC.HsRecFields (GhcPass p) arg)
compileRecFields f fields = do
  fields' <-
    sequence
      [ do
          field' <- compileFieldOcc field
          x' <- f x
          pure . genLoc $
            GHC.HsFieldBind
              { hfbAnn = GHC.noAnn
              , hfbLHS = genLoc field'
              , hfbRHS = x'
              , hfbPun = False
              }
      | (field, x) <- fields
      ]
  pure
    GHC.HsRecFields
      { rec_flds = fields'
      , rec_dotdot = Nothing
      }
  where
    compileFieldOcc field = do
      name <- compileHsName field
      pure $
        onPsOrRn @p
          GHC.FieldOcc
            { foExt = GHC.noExtField
            , foLabel = genLoc name
            }
          GHC.FieldOcc
            { foExt = name
            , foLabel = genLoc $ GHC.getRdrName name
            }

genLocConLikeP ::
  forall p.
  (IsPass p) =>
  GHC.IdP (GhcPass p)
  -> GHC.XRec (GhcPass p) (GHC.ConLikeP (GhcPass p))
genLocConLikeP idp = onPsOrRn @p (genLoc idp) (genLoc idp)

genLocIdP ::
  forall p.
  (IsPass p) =>
  GHC.IdP (GhcPass p)
  -> GHC.LIdP (GhcPass p)
genLocIdP idp = onPsOrRn @p (genLoc idp) (genLoc idp)
