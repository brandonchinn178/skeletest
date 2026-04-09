{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Skeletest.Internal.Hooks (
  Hooks (..),
  defaultHooks,

  -- * Runtime
  UserHooks,
  setUserHooks,
  userHooks,

  -- * Hook implementation
  Hook (..),
  HookDef (..),
  HookPriority (..),
  runHook,

  -- * Hook DSL
  runEarly,
  runLate,
  mkHook,
  mkHook_,
  mkPreHook,
  mkPreHook_,
  mkPostHook,
  mkPostHook_,

  -- * Specific hooks

  -- ** modifySpecRegistry
  ModifySpecRegistryHook,
  ModifySpecRegistryHookContext (..),

  -- ** runTest
  RunTestHook,
  RunTestHookContext (..),

  -- ** onTestFailure
  OnTestFailureHook,
  OnTestFailureHookContext (..),

  -- ** runSpecs
  RunSpecsHook,
  RunSpecsHookContext (..),

  -- ** modifyTestSummary
  ModifyTestSummaryHook,
  ModifyTestSummaryHookContext (..),
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import GHC.Records (HasField (..))
import Skeletest.Internal.Exit (TestExitCode)
import Skeletest.Internal.Hooks.HookDef
import Skeletest.Internal.Spec.Tree (SpecRegistry)
import Skeletest.Internal.TestInfo (TestInfo)
import Skeletest.Internal.TestRunner (TestResult)
import Skeletest.Internal.TestTargets (TestTargets)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (SomeException)

-- | Hooks for extending Skeletest.
--
-- Use 'defaultHooks' instead of using v'Hooks' directly, to minimize
-- breaking changes.
data Hooks = Hooks
  { modifySpecRegistry :: ModifySpecRegistryHook
  , runTest :: RunTestHook
  , onTestFailure :: OnTestFailureHook
  , runSpecs :: RunSpecsHook
  , modifyTestSummary :: ModifyTestSummaryHook
  }

instance Semigroup Hooks where
  hooks1 <> hooks2 =
    Hooks
      { modifySpecRegistry = hooks1.modifySpecRegistry <> hooks2.modifySpecRegistry
      , runTest = hooks1.runTest <> hooks2.runTest
      , onTestFailure = hooks1.onTestFailure <> hooks2.onTestFailure
      , runSpecs = hooks1.runSpecs <> hooks2.runSpecs
      , modifyTestSummary = hooks1.modifyTestSummary <> hooks2.modifyTestSummary
      }
instance Monoid Hooks where
  mempty = defaultHooks

defaultHooks :: Hooks
defaultHooks =
  Hooks
    { modifySpecRegistry = mempty
    , runTest = mempty
    , onTestFailure = mempty
    , runSpecs = mempty
    , modifyTestSummary = mempty
    }

{----- Runtime -----}

userHooksRef :: IORef Hooks
userHooksRef = unsafePerformIO $ newIORef defaultHooks

setUserHooks :: Hooks -> IO ()
setUserHooks = writeIORef userHooksRef

userHooks :: UserHooks
userHooks = unsafePerformIO $ UserHooks <$> readIORef userHooksRef

newtype UserHooks = UserHooks Hooks

instance
  (HasField field Hooks (Hook ctx inp out)) =>
  HasField field UserHooks (ctx -> inp -> (inp -> IO out) -> IO out)
  where
  getField (UserHooks hooks) = runHook (getField @field hooks)

{----- modifySpecRegistry -----}

-- | Modify the specs in the test suite.
type ModifySpecRegistryHook =
  Hook
    ModifySpecRegistryHookContext
    SpecRegistry
    SpecRegistry

data ModifySpecRegistryHookContext = ModifySpecRegistryHookContext
  { testTargets :: TestTargets
  }

{----- runTest -----}

-- | Modify how a test is executed
type RunTestHook =
  Hook
    RunTestHookContext
    ()
    TestResult

data RunTestHookContext = RunTestHookContext
  { testInfo :: TestInfo
  }

{----- onTestFailure -----}

-- | Modify what happens if a test fails.
type OnTestFailureHook =
  Hook
    OnTestFailureHookContext
    SomeException
    TestResult

data OnTestFailureHookContext = OnTestFailureHookContext
  { testInfo :: TestInfo
  }

{----- runSpecs -----}

-- | Modify the action to run specs.
type RunSpecsHook =
  Hook
    RunSpecsHookContext
    SpecRegistry
    TestExitCode

data RunSpecsHookContext = RunSpecsHookContext
  {
  }

{----- modifyTestSummary -----}

-- | Modify the test summary at the end of the report.
type ModifyTestSummaryHook =
  Hook
    ModifyTestSummaryHookContext
    Text
    Text

data ModifyTestSummaryHookContext = ModifyTestSummaryHookContext
  {
  }
