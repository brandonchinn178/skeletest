{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.Plugin (
  plugin,
) where

import Data.Function ((&))

import Skeletest.Internal.Constants (mainFileSpecsListIdentifier)
import Skeletest.Internal.Error (skeletestPluginError)
import Skeletest.Internal.GHC

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
        , funType = HsTypeApp (HsTypeCon "IO") [HsTypeTuple []]
        , funPats = []
        , funBody =
            HsExprApp
              (HsExprVar "runSkeletest")
              [ HsExprRecordCon
                  "SkeletestOptions"
                  [ ("plugins", pluginsExpr)
                  , ("snapshotRenderers", snapshotRenderersExpr)
                  ]
              , HsExprVar mainFileSpecsListIdentifier
              ]
        }

-- | If a module does not export a 'spec' identifier (e.g. if the module
-- only contains test utilities), add an empty spec.
transformTestModule :: ParsedModule -> ParsedModule
transformTestModule modl
  -- if spec is not defined, generate one
  | not isSpecDefined =
      modl
        & addModuleFun specFun
        & addModuleExport (ModuleExportVar "spec")
  -- if spec is defined but not exported, error
  | not isSpecExported =
      skeletestPluginError . unlines $
        [ "`spec` is defined as a top-level value but not exported."
        , "If this is a test file, export it. Otherwise, rename it."
        ]
  -- if spec is defined + exported, we're good to go
  | otherwise = modl
  where
    isSpecDefined = ModuleValFun "spec" `elem` getModuleVals modl
    isSpecExported =
      case getModuleExports modl of
        ModuleExportEverything -> True
        ModuleExportList exports -> ModuleExportVar "spec" `elem` exports

    specFun =
      FunDef
        { funName = "spec"
        , funType = HsTypeCon "Spec"
        , funPats = []
        , funBody = HsExprApp (HsExprVar "pure") [HsExprTuple []]
        }
