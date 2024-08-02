{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Skeletest.Internal.Plugin (
  plugin,
) where

import Data.Functor.Const (Const (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as Text

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

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
      , modifyParsed = \modName modl ->
          if modName == "Main"
            then transformMainModule modl
            else modl
      , onRename = \ctx modName expr ->
          if "Spec" `Text.isSuffixOf` modName
            then transformTestModule ctx expr
            else expr
      }

-- | Add 'main' function.
transformMainModule :: ParsedModule -> ParsedModule
transformMainModule modl = modl{moduleFuncs = (hsVarName "main", Just mainFun) : moduleFuncs modl}
  where
    findVar name =
      fmap hsExprVar . listToMaybe $
        [ funName
        | (funName, _) <- moduleFuncs modl
        , getHsName funName == name
        ]

    cliFlagsExpr = fromMaybe (hsExprList []) $ findVar "cliFlags"
    snapshotRenderersExpr = fromMaybe (hsExprList []) $ findVar "snapshotRenderers"
    hooksExpr = fromMaybe (hsExprVar $ hsName 'Plugin.defaultHooks) $ findVar "hooks"
    pluginsExpr = fromMaybe (hsExprList []) $ findVar "plugins"

    mainFun =
      FunDef
        { funType = HsTypeApps (HsTypeCon $ hsName ''IO) [HsTypeTuple []]
        , funPats = []
        , funBody =
            hsExprApps
              (hsExprVar $ hsName 'Main.runSkeletest)
              [ hsExprApps (hsExprVar (hsName '(:))) $
                  [ hsExprRecordCon
                      (hsName 'Plugin.Plugin)
                      [ (hsName 'Plugin.cliFlags, cliFlagsExpr)
                      , (hsName 'Plugin.snapshotRenderers, snapshotRenderersExpr)
                      , (hsName 'Plugin.hooks, hooksExpr)
                      ]
                  , pluginsExpr
                  ]
              , hsExprVar $ hsVarName mainFileSpecsListIdentifier
              ]
        }

transformTestModule :: Ctx -> HsExpr GhcRn -> HsExpr GhcRn
transformTestModule ctx =
  foldl' (.) id $
    [ replaceConMatch ctx
    , replaceIsoChecker ctx
    ]

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
replaceConMatch :: Ctx -> HsExpr GhcRn -> HsExpr GhcRn
replaceConMatch ctx e =
  case getExpr e of
    -- Matches:
    --   P.con User{name = ...}
    --   P.con (User "...")
    HsExprApps (getExpr -> HsExprVar name) [arg]
      | isCon name ->
          convertCon arg
    -- Matches:
    --   P.con $ User "..."
    HsExprOp (getExpr -> HsExprVar name) (getExpr -> HsExprVar dollar) arg
      | matchesName ctx (hsName '($)) dollar
      , isCon name ->
          convertCon arg
    -- Check if P.con is by itself
    HsExprVar name
      | isCon name ->
          skeletestPluginError "P.con must be applied to a constructor"
    -- Check if P.con is being applied more than once
    HsExprApps (getExpr -> HsExprVar name) (_ : _ : _)
      | isCon name ->
          skeletestPluginError "P.con must be applied to exactly one argument"
    _ -> e
  where
    isCon = matchesName ctx (hsName 'P.con)

    convertCon con =
      case getExpr con of
        HsExprCon conName -> convertPrefixCon conName []
        HsExprApps (getExpr -> HsExprCon conName) preds -> convertPrefixCon conName preds
        HsExprRecordCon conName fields -> convertRecordCon conName fields
        _ -> skeletestPluginError "P.con must be applied to a constructor"
    convertPrefixCon conName preds =
      let
        exprNames = mkVarNames preds
       in
        hsExprApps (hsExprVar $ hsName 'P.conMatches) $
          [ hsExprLitString $ getHsName conName
          , hsExprCon $ hsName 'Nothing
          , mkDeconstruct (HsPatCon conName $ map HsPatVar exprNames) exprNames
          , mkPredList preds
          ]
    convertRecordCon conName fields =
      let
        (fieldNames, preds) = unzip fields
        fieldPats = [(field, HsPatVar field) | field <- fieldNames]
       in
        hsExprApps (hsExprVar $ hsName 'P.conMatches) $
          [ hsExprLitString $ getHsName conName
          , hsExprApps (hsExprCon $ hsName 'Just) [mkNamesList fieldNames]
          , mkDeconstruct (HsPatRecord conName fieldPats) fieldNames
          , mkPredList preds
          ]

    -- Generate variable names like x0, x1, ... for each element in the given list.
    mkVarNames =
      let mkVar i = "x" <> (Text.pack . show) i
       in zipWith (\i _ -> hsVarName (mkVar i)) [0 :: Int ..]

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
      hsExprLam [HsPatVar $ hsVarName "actual"] $
        hsExprCase (hsExprApps (hsExprVar $ hsName 'pure) [hsExprVar $ hsVarName "actual"]) $
          [ (HsPatCon (hsName 'Just) [pat], hsExprApps (hsExprCon $ hsName 'Just) [mkValsList argNames])
          , (HsPatWild, hsExprCon $ hsName 'Nothing)
          ]

    mkHList f = \case
      [] -> hsExprCon (hsName 'HNil)
      x : xs ->
        hsExprApps (hsExprCon $ hsName 'HCons) $
          [ f x
          , mkHList f xs
          ]

    mkNamesList = mkHList $ \name -> hsExprApps (hsExprCon $ hsName 'Const) [hsExprLitString $ getHsName name]
    mkValsList = mkHList $ \val -> hsExprApps (hsExprVar $ hsName 'pure) [hsExprVar val]
    mkPredList = mkHList id

-- | Replace all uses of P.=== with inlined IsoChecker value, with
-- function name filled in.
--
-- (encode . decode) P.=== id
-- ====>
-- IsoChecker (Fun "encode . decode" (encode . decode)) (Fun "id" id)
replaceIsoChecker :: Ctx -> HsExpr GhcRn -> HsExpr GhcRn
replaceIsoChecker ctx e =
  case getExpr e of
    HsExprOp l (getExpr -> HsExprVar eqeqeq) r
      | matchesName ctx (hsName '(P.===)) eqeqeq ->
          inlineIsoChecker l r
    _ -> e
  where
    inlineIsoChecker l r = hsExprApps (hsExprCon $ hsName 'P.IsoChecker) [mkFun l, mkFun r]
    mkFun f = hsExprApps (hsExprCon $ hsName 'P.Fun) [hsExprLitString $ renderHsExpr f, f]
