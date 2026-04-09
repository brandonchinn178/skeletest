{-# LANGUAGE DuplicateRecordFields #-}

-- | The module that contains everything needed to implement custom hooks.
module Skeletest.Hooks (
  -- * Hooks
  X.Hooks (..),
  X.Hook,
  X.defaultHooks,

  -- ** Specific hooks

  -- *** modifySpecRegistry
  X.ModifySpecRegistryHook,
  X.ModifySpecRegistryHookContext (..),

  -- *** runTest
  X.RunTestHook,
  X.RunTestHookContext (..),

  -- *** onTestFailure
  X.OnTestFailureHook,
  X.OnTestFailureHookContext (..),

  -- *** runSpecs
  X.RunSpecsHook,
  X.RunSpecsHookContext (..),

  -- *** modifyTestSummary
  X.ModifyTestSummaryHook,
  X.ModifyTestSummaryHookContext (..),

  -- ** Implementing a hook
  X.runEarly,
  X.runLate,
  X.mkHook,
  X.mkHook_,
  X.mkPreHook,
  X.mkPreHook_,
  X.mkPostHook,
  X.mkPostHook_,

  -- * Re-exports

  -- ** TestResult
  X.TestResult (..),
  X.TestResultMessage (..),
  X.BoxSpec,
  X.BoxSpecContent (..),

  -- ** TestInfo
  X.TestInfo (..),

  -- ** Markers
  X.findMarker,
  X.hasMarker,
  X.hasMarkerNamed,

  -- ** SpecRegistry
  X.SpecRegistry,
  X.Spec,
  X.SpecInfo (..),
  X.SpecTree (..),
  X.SpecTest (..),
  X.getSpecTrees,
  X.withSpecTrees,
  X.mapSpecTrees,
  X.traverseSpecTrees,
  X.mapSpecTests,
  X.traverseSpecTests,
  X.filterSpecTests,
  X.mapSpecs,
  X.traverseSpecs,
) where

import Skeletest.Internal.Hooks qualified as X
import Skeletest.Internal.Markers qualified as X
import Skeletest.Internal.Spec.Output qualified as X
import Skeletest.Internal.Spec.Tree qualified as X
import Skeletest.Internal.TestInfo qualified as X
import Skeletest.Internal.TestRunner qualified as X
