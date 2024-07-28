{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Skeletest.Internal.Plugin (
  plugin,
) where

import Data.Functor.Const (Const (..))
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text

import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (skeletestPluginError)
import Skeletest.Internal.GHC
import Skeletest.Internal.Predicate qualified as P
import Skeletest.Internal.Utils.HList (HList (..))
import Skeletest.Main qualified as Main
import Skeletest.Plugin qualified as Plugin

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
    moduleVals = map moduleValName $ getModuleVals modl
    getVal name = listToMaybe $ filter ((== name) . getHsName) moduleVals
    varOr name def = maybe def HsExprVar (getVal name)

    cliFlagsExpr = varOr "cliFlags" (HsExprList [])
    snapshotRenderersExpr = varOr "snapshotRenderers" (HsExprList [])
    pluginsExpr = varOr "plugins" (HsExprList [])

    mainFun =
      FunDef
        { funName = "main"
        , funType = HsTypeApp (HsTypeCon $ hsName ''IO) [HsTypeTuple []]
        , funPats = []
        , funBody =
            hsApps
              (HsExprVar $ hsName 'Main.runSkeletest)
              [ hsApps (HsExprVar (hsName '(:))) $
                  [ HsExprRecordCon
                      (hsName 'Plugin.Plugin)
                      [ (hsName 'Plugin.cliFlags, cliFlagsExpr)
                      , (hsName 'Plugin.snapshotRenderers, snapshotRenderersExpr)
                      ]
                  , pluginsExpr
                  ]
              , HsExprVar $ hsNewName mainFileSpecsListIdentifier
              ]
        }

transformTestModule :: ParsedModule -> ParsedModule
transformTestModule = replaceConMatch . replaceIsoChecker

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
      -- Matches:
      --   P.con User{name = ...}
      --   P.con (User "...")
      HsExprApp (HsExprVar name) arg
        | isCon name ->
            convertCon arg
      -- Matches:
      --   P.con $ User "..."
      HsExprOp (HsExprVar name) (HsExprVar dollar) arg
        | getHsName dollar == "$"
        , isCon name ->
            convertCon arg
      -- Check if P.con is by itself
      HsExprVar name
        | isCon name ->
            skeletestPluginError "P.con must be applied to a constructor"
      -- Check if P.con is being applied more than once
      HsExprApp (HsExprApp (HsExprVar name) _) _
        | isCon name ->
            skeletestPluginError "P.con must be applied to exactly one argument"
      _ -> Nothing

    -- TODO: Make this more precise. It seems like the only information we get here is P.con,
    -- but it might be possible to look up what modules were imported as "P".
    isCon n = getHsName n == "con" && getHsNameMod n == Just "P"

    convertCon = \case
      arg | (HsExprCon conName, preds) <- collectApps arg -> do
        let exprNames = mkVarNames preds
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

    -- Generate variable names like x0, x1, ... for each element in the given list.
    mkVarNames =
      let mkVar i = "x" <> (Text.pack . show) i
       in zipWith (\i _ -> hsNewName (mkVar i)) [0 :: Int ..]

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

-- | Replace all uses of P.=== with inlined IsoChecker value, with
-- function name filled in.
--
-- (encode . decode) P.=== id
-- ====>
-- IsoChecker (Fun "encode . decode" (encode . decode)) (Fun "id" id)
replaceIsoChecker :: ParsedModule -> ParsedModule
replaceIsoChecker parsedModule = modifyModuleExprs go parsedModule
  where
    go = \case
      HsExprOp l (HsExprVar eqeqeq) r | getHsName eqeqeq == "===" -> Just $ inlineIsoChecker l r
      _ -> Nothing

    inlineIsoChecker l r = hsApps (HsExprCon $ hsName 'P.IsoChecker) [mkFun l, mkFun r]
    mkFun f = hsApps (HsExprCon $ hsName 'P.Fun) [HsExprLitString $ renderHsExpr parsedModule f, f]
