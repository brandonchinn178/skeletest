{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.Hooks.HookDef (
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
) where

import Control.Monad ((>=>))
import Data.List (sortOn)

-- | The implementation of a Skeletest hook in 'Skeletest.Plugin.Hooks'.
--
-- A hook of type @Hook ctx inp out@ means:
--
--   * The hook takes a value of type @ctx@ with extra read-only information
--     that may be relevant for the hook.
--   * The hook takes an action of type @inp -> IO out@ and returns a
--     potentially modified action of the same type.
--
-- @ctx@/@inp@ should generally be accessed with @OverloadedRecordDot@ or record
-- destructuring, to minimize breaking changes when adding fields to them.
newtype Hook ctx inp out = Hook [HookDef ctx inp out]
  deriving newtype (Monoid, Semigroup)

data HookDef ctx inp out = HookDef
  { priority :: HookPriority
  , impl :: ctx -> (inp -> IO out) -> (inp -> IO out)
  }

data HookPriority
  = NoPriority
  | EarlyPriority
  | LatePriority

defaultHookDef :: HookDef ctx inp out
defaultHookDef =
  HookDef
    { priority = NoPriority
    , impl = \_ run -> run
    }

runHook :: Hook ctx inp out -> ctx -> inp -> (inp -> IO out) -> IO out
runHook (Hook hookDefs) ctx inp run = foldr go run (orderHookDefs hookDefs) inp
 where
  orderHookDefs = sortOn $ \def ->
    case def.priority of
      NoPriority -> 0 :: Int
      EarlyPriority -> -1
      LatePriority -> 1
  go hookDef acc = hookDef.impl ctx acc

{----- DSL -----}

-- | Run the given hook earlier if possible.
runEarly :: Hook ctx inp out -> Hook ctx inp out
runEarly (Hook hookDefs) = Hook (map (\def -> def{priority = EarlyPriority}) hookDefs)

-- | Run the given hook later if possible.
runLate :: Hook ctx inp out -> Hook ctx inp out
runLate (Hook hookDefs) = Hook (map (\def -> def{priority = LatePriority}) hookDefs)

-- | Create a hook.
--
-- === Example
-- @
-- hooks :: Hooks
-- hooks =
--   defaultHooks
--     { runSpecs = runEarly . mkHook $ \ctx run -> pre >=> run >=> post
--     }
--   where
--     pre inp = do
--       inp' <- doStuff inp
--       pure inp'
--     post out = do
--       out' <- doMoreStuff out
--       pure out'
-- @
mkHook :: (ctx -> (inp -> IO out) -> (inp -> IO out)) -> Hook ctx inp out
mkHook f = Hook [defaultHookDef{impl = f}]

-- | Like 'mkHook', except for read-only hooks that don't need to modify
-- anything.
mkHook_ ::
  (ctx -> inp -> IO ()) ->
  (ctx -> inp -> out -> IO ()) ->
  Hook ctx inp out
mkHook_ pre post = mkHook $ \ctx run inp -> do
  pre ctx inp
  out <- run inp
  post ctx inp out
  pure out

-- | Like 'mkHook', except only supports modifying the input.
mkPreHook :: (ctx -> inp -> IO inp) -> Hook ctx inp out
mkPreHook pre = mkHook $ \ctx run -> pre ctx >=> run

-- | Like 'mkHook_', except with only a before action.
mkPreHook_ :: (ctx -> inp -> IO ()) -> Hook ctx inp out
mkPreHook_ pre = mkHook_ pre (\_ _ _ -> pure ())

-- | Like 'mkHook', except only supports modifying the output.
mkPostHook :: (ctx -> inp -> out -> IO out) -> Hook ctx inp out
mkPostHook post = mkHook $ \ctx run inp -> run inp >>= post ctx inp

-- | Like 'mkHook_', except with only an after action.
mkPostHook_ :: (ctx -> inp -> out -> IO ()) -> Hook ctx inp out
mkPostHook_ post = mkHook_ (\_ _ -> pure ()) post
