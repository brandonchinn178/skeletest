{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Skeletest.Internal.Plugin (
  plugin,
) where

import Data.Function ((&))
import Data.Functor.Const (Const (..))
import Data.Text qualified as Text

import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (skeletestPluginError)
import Skeletest.Internal.GHC
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.Spec (Spec)
import Skeletest.Internal.Utils.HList (HList (..))
import Skeletest.Main qualified as Main

-- | The plugin to convert a module in the tests directory.
-- Injected by the preprocessor.
plugin :: Plugin
plugin =
  mkPlugin
    PluginDef
      { isPure = True
      , afterParse = \modName modl ->
          if modName == "Main"
            then transformMainModule modl
            else transformTestModule modl
      }

-- | Add 'main' function.
transformMainModule :: ParsedModule -> ParsedModule
transformMainModule modl = addModuleFun mainFun modl
  where
    -- TODO: if `plugins` value exists in `getModuleVals modl`, use `HsExprVar "plugins"`
    pluginsExpr = HsExprList []

    -- TODO: if `snapshotRenderers` value exists in `getModuleVals modl`, use `HsExprVar "snapshotRenderers"`
    snapshotRenderersExpr = HsExprList []

    mainFun =
      FunDef
        { funName = "main"
        , funType = HsTypeApp (HsTypeCon $ hsName ''IO) [HsTypeTuple []]
        , funPats = []
        , funBody =
            hsApps
              (HsExprVar $ hsName 'Main.runSkeletest)
              [ HsExprRecordCon
                  (hsName 'Main.SkeletestOptions)
                  [ (hsName 'Main.plugins, pluginsExpr)
                  , (hsName 'Main.snapshotRenderers, snapshotRenderersExpr)
                  ]
              , HsExprVar $ hsNewName mainFileSpecsListIdentifier
              ]
        }

transformTestModule :: ParsedModule -> ParsedModule
transformTestModule = replaceConMatch . addSpec

-- | Replace all uses of P.con with P.conMatches. See P.con.
--
-- P.con $ User (P.eq "user1") (P.contains "@")
-- ====>
-- P.conMatches
--   "User"
--   Nothing
--   ( \case
--       User x0 x1 -> Just (HCons (pure x0) $ HCons (pure x1) $ HNil)
--       _ -> Nothing
--   )
--   (HCons (H.eq "user1") $ HCons (P.contains "@") $ HNil)
--
-- P.con User{name = P.eq "user1", email = P.contains "@"}
-- ====>
-- P.conMatches
--   "User"
--   (Just (HCons (Const "user") $ HCons (Const "email") $ HNil))
--   ( \case
--       User{name, email} -> Just (HCons (pure name) $ HCons (pure email) $ HNil)
--       _ -> Nothing
--   )
--   (HCons (P.eq "user1") $ HCons (P.contains "@") $ HNil)
replaceConMatch :: ParsedModule -> ParsedModule
replaceConMatch = modifyModuleExprs go
  where
    go = \case
      HsExprApp (HsExprVar name) arg | isCon name ->
        case arg of
          _ | (HsExprCon conName, preds) <- collectApps arg -> do
            let exprNames = zipWith (\_ i -> hsNewName . Text.pack . show $ i) preds [0 :: Int ..]
            Just . hsApps (HsExprVar $ hsName 'P.conMatches) $
              [ HsExprLitString $ renderHsName conName
              , HsExprCon $ hsName 'Nothing
              , mkDeconstruct (HsPatCon conName $ map HsPatVar exprNames) exprNames
              , mkPredList preds
              ]
          HsExprRecordCon conName fields -> do
            let (fieldNames, preds) = unzip fields
                fieldPats = [(field, HsPatVar field) | field <- fieldNames]
            Just . hsApps (HsExprVar $ hsName 'P.conMatches) $
              [ HsExprLitString $ renderHsName conName
              , HsExprApp (HsExprCon $ hsName 'Just) (mkNamesList fieldNames)
              , mkDeconstruct (HsPatRecord conName fieldPats) fieldNames
              , mkPredList preds
              ]
          _ -> skeletestPluginError "P.con must be applied to a constructor"
      -- Check if P.con is being applied more than once
      HsExprApp (HsExprApp (HsExprVar name) _) _ | isCon name ->
        skeletestPluginError "P.con must be applied to exactly one argument"
      _ -> Nothing

    -- TODO: Make this more precise. It seems like the only information we get here is P.con,
    -- but it might be possible to look up what modules were imported as "P".
    isCon n = getHsName n == "con" && getHsNameMod n == Just "P"

    -- Create the deconstruction function:
    --
    -- \actual ->
    --   case actual of
    --     User{name} -> Just (HCons (pure name) HNil)
    --     _ -> Nothing
    --
    -- However, if 'User' is the only constructor, GHC complains about the wildcard
    -- being redundant. So we'll obfuscate it a bit with
    --
    -- \actual ->
    --   case pure actual of
    --     Just User{name} -> Just (HCons (pure name) HNil)
    --     _ -> Nothing
    mkDeconstruct pat argNames =
      HsExprLam (HsPatVar $ hsNewName "actual") $
        HsExprCase (HsExprVar (hsName 'pure) `HsExprApp` HsExprVar (hsNewName "actual")) $
          [ (HsPatCon (hsName 'Just) [pat], HsExprApp (HsExprCon $ hsName 'Just) (mkValsList argNames))
          , (HsPatWild, HsExprCon $ hsName 'Nothing)
          ]

    mkHList f = \case
      [] -> HsExprCon (hsName 'HNil)
      x : xs ->
        HsExprCon (hsName 'HCons)
          `HsExprApp` f x
          `HsExprApp` mkHList f xs

    mkNamesList = mkHList $ \name -> HsExprApp (HsExprCon $ hsName 'Const) (HsExprLitString $ renderHsName name)
    mkValsList = mkHList $ \val -> HsExprApp (HsExprVar $ hsName 'pure) (HsExprVar val)
    mkPredList = mkHList id

-- | If a module does not export a 'spec' identifier (e.g. if the module
-- only contains test utilities), add an empty spec.
addSpec :: ParsedModule -> ParsedModule
addSpec modl
  -- if spec is not defined, generate one
  | not isSpecDefined =
      modl
        & addModuleFun specFun
        & addModuleExport (ModuleExportVar $ hsNewName "spec")
  -- if spec is defined but not exported, error
  | not isSpecExported =
      skeletestPluginError . unlines $
        [ "`spec` is defined as a top-level value but not exported."
        , "If this is a test file, export it. Otherwise, rename it."
        ]
  -- if spec is defined + exported, we're good to go
  | otherwise = modl
  where
    isSpecDefined = any (== "spec") [getHsName name | ModuleValFun name <- getModuleVals modl]
    isSpecExported =
      case getModuleExports modl of
        ModuleExportEverything -> True
        ModuleExportList exports -> any (== "spec") [getHsName name | ModuleExportVar name <- exports]

    specFun =
      FunDef
        { funName = "spec"
        , funType = HsTypeCon $ hsName ''Spec
        , funPats = []
        , funBody = hsApps (HsExprVar $ hsName 'pure) [HsExprTuple []]
        }
