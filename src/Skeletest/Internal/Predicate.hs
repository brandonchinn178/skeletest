{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}

module Skeletest.Internal.Predicate (
  Predicate,
  PredicateResult (..),
  runPredicate,
  renderPredicate,

  -- * General
  anything,

  -- * Ord
  eq,
  gt,

  -- * Data types
  just,
  -- TODO: nothing,
  -- tup2,
  -- tup3,
  -- tup4,
  -- tup,
  con,
  conMatches,

  -- * Numeric
  approx,
  tol,
  Tolerance (..),

  -- * Combinators
  (<<<),
  (>>>),
  not,
  -- TODO: P.any, P.all

  -- * Containers
  -- TODO: P.contains (Containable: [a], Text)

  -- * IO
  returns,
  -- TODO: throws

  -- * Snapshot testing
  matchesSnapshot,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef (atomicModifyIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Debug.RecoverRTTI (anythingToString)
import GHC.Generics ((:*:) (..))
import Prelude hiding (abs, not)
import Prelude qualified

import Skeletest.Internal.Fixtures (getFixture)
import Skeletest.Internal.Snapshot (
  SnapshotContext (..),
  SnapshotFixture (..),
  SnapshotResult (..),
  checkSnapshot,
  defaultSnapshotRenderers,
 )
import Skeletest.Internal.State (getTestInfo)
import Skeletest.Internal.Utils.Diff (showLineDiff)
import Skeletest.Internal.Utils.HList (HList (..))
import Skeletest.Internal.Utils.HList qualified as HList

data Predicate a = Predicate
  { predicateFunc :: a -> IO PredicateFuncResult
  , predicateDisp :: Text
    -- ^ The rendered representation of the predicate
  , predicateDispNeg :: Text
    -- ^ The rendered representation of the negation of the predicate
  }

data PredicateResult
  = PredicateSuccess
  | PredicateFail Text

runPredicate :: Predicate a -> a -> IO PredicateResult
runPredicate Predicate{..} val = do
  PredicateFuncResult{..} <- predicateFunc val
  pure $
    if predicateSuccess
      then PredicateSuccess
      else
        PredicateFail $
          if Prelude.not predicateNested
            then predicateFailMsg
            else
              Text.intercalate "\n" $
                [ predicateFailMsg
                , "Expected:"
                , indent predicateDisp
                , "Got:"
                , indent $ render val
                ]
  where
    indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"

renderPredicate :: Predicate a -> Text
renderPredicate = predicateDisp

data PredicateFuncResult = PredicateFuncResult
  { predicateSuccess :: Bool
  , predicateFailMsg :: Text
    -- ^ The message to show on failure
  , predicatePassMsg :: Text
    -- ^ The message to show on unexpected pass
  , predicateNested :: Bool
    -- ^ Did this result come from a nested predicate?
  }

setNested :: PredicateFuncResult -> PredicateFuncResult
setNested result = result{predicateNested = True}

{----- General -----}

anything :: Predicate a
anything =
  Predicate
    { predicateFunc = \_ ->
        pure
          PredicateFuncResult
            { predicateSuccess = True
            , predicateFailMsg = "anything"
            , predicatePassMsg = "not anything"
            , predicateNested = False
            }
    , predicateDisp = "anything"
    , predicateDispNeg = "not anything"
    }

{----- Ord -----}

-- TODO: if rendered vals are too long, show a diff
eq :: Eq a => a -> Predicate a
eq = mkPredicateOp "=" "≠" $ \actual expected -> actual == expected

gt :: Ord a => a -> Predicate a
gt = mkPredicateOp ">" "≯" $ \actual expected -> actual > expected

{----- Data types -----}

just :: Predicate a -> Predicate (Maybe a)
just Predicate{..} =
  Predicate
    { predicateFunc = \case
        Just a -> setNested <$> predicateFunc a
        Nothing ->
          pure
            PredicateFuncResult
              { predicateSuccess = False
              , predicateFailMsg = "Nothing ≠ " <> disp
              , predicatePassMsg = "Nothing = " <> disp
              , predicateNested = False
              }
    , predicateDisp = disp
    , predicateDispNeg = "not " <> disp
    }
  where
    disp = "Just (" <> predicateDisp <> ")"

-- -- | A predicate for checking that the given tuple matches the given predicates.
-- tup2 :: (Predicate a, Predicate b) -> Predicate (a, b)
-- tup2 (predA, predB) =
--   Predicate
--     { predicateFunc = \(a, b) ->
--         predicateFunc predA a
--     }

-- | A predicate for checking that a value matches the given constructor.
--
-- It takes one argument, which is the constructor, except with all fields
-- taking a Predicate instead of the normal value. Skeletest will rewrite
-- the expression so it typechecks correctly.
--
-- >>> user `shouldSatisfy` P.con User{name = P.eq "user1", email = P.contains "@"}
--
-- Record fields that are omitted are not checked at all; i.e.
-- @P.con Foo{}@ and @P.con Foo{a = P.anything}@ are equivalent.
con :: a -> Predicate a
con =
  -- A placeholder that will be replaced with conMatches in the plugin.
  error "P.con was not replaced"

-- | A predicate for checking that a value matches the given constructor.
-- Assumes that the arguments correctly match the constructor being tested,
-- so it should not be written directly, only generated from `con`.
conMatches ::
  String
  -> Maybe (HList (Const String) fields)
  -> (a -> Maybe (HList Identity fields))
  -> HList Predicate fields
  -> Predicate a
conMatches conNameS mFieldNames deconstruct preds =
  Predicate
    { predicateFunc = \actual ->
        case deconstruct actual of
          Just fields -> do
            runExceptT (HList.toListWithM runPred $ HList.hzip fields preds) >>= \case
              Left result -> pure $ setNested result
              Right _ -> pure $ mkResult True actual predsDisp
          Nothing -> pure $ mkResult False actual conName
    , predicateDisp = "matches " <> predsDisp
    , predicateDispNeg = "does not match " <> predsDisp
    }
  where
    conName = Text.pack conNameS
    runPred (Identity field :*: p) = do
      result@PredicateFuncResult{..} <- liftIO $ predicateFunc p field
      if predicateSuccess
        then pure result
        else throwE result

    mkResult success actual rhs =
      PredicateFuncResult
        { predicateSuccess = success
        , predicateFailMsg = render actual <> " ≠ " <> rhs
        , predicatePassMsg = render actual <> " = " <> rhs
        , predicateNested = False
        }

    predsDisp =
      case mFieldNames of
        Just fieldNames ->
          let
            renderField (Const fieldName :*: p) = Text.pack fieldName <> " = " <> parens (predicateDisp p)
            fields = HList.toListWith renderField $ HList.hzip fieldNames preds
          in
            conName <> "{" <> Text.intercalate ", " fields <> "}"
        Nothing ->
          Text.unwords $ conName : map parens (HList.toListWith predicateDisp preds)

{----- Numeric -----}

-- | A predicate for checking that a value is equal within some tolerance.
--
-- Useful for checking equality with floats, which might not be exactly equal.
-- For more information, see: https://jvns.ca/blog/2023/01/13/examples-of-floating-point-problems/.
--
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.abs = 1e-12} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6, P.abs = 1e-12} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Nothing} 0.3
-- >>> 0.1 + 0.2 `shouldSatisfy` P.approx P.tol{P.rel = Nothing, P.abs = 1e-12} 0.3
approx :: (Fractional a, Ord a) => Tolerance -> a -> Predicate a
approx Tolerance{..} =
  mkPredicateOp "≈" "≉" $ \actual expected ->
    Prelude.abs (actual - expected) <= getTolerance expected
  where
    mRelTol = fromTol <$> rel
    absTol = fromTol abs
    getTolerance expected =
      case mRelTol of
        Just relTol -> max (relTol * Prelude.abs expected) absTol
        Nothing -> absTol

    fromTol x
      | x < 0 = error $ "tolerance can't be negative: " <> show x
      | otherwise = fromRational x

data Tolerance = Tolerance
  { rel :: Maybe Rational
  , abs :: Rational
  }

tol :: Tolerance
tol = Tolerance{rel = Just 1e-6, abs = 1e-12}

{----- Combinators -----}

(<<<) :: Predicate a -> (b -> a) -> Predicate b
Predicate{..} <<< f =
  -- TODO: render function name in predicateDisp?
  Predicate
    { predicateFunc = fmap setNested . predicateFunc . f
    , predicateDisp
    , predicateDispNeg
    }

(>>>) :: (b -> a) -> Predicate a -> Predicate b
(>>>) = flip (<<<)

not :: Predicate a -> Predicate a
not Predicate{..} =
  Predicate
    { predicateFunc = \actual -> do
        PredicateFuncResult{..} <- predicateFunc actual
        pure
          PredicateFuncResult
            { predicateSuccess = Prelude.not predicateSuccess
            , predicateFailMsg = predicatePassMsg
            , predicatePassMsg = predicateFailMsg
            , predicateNested = True
            }
    , predicateDisp = predicateDispNeg
    , predicateDispNeg = predicateDisp
    }

{----- IO -----}

returns :: Predicate a -> Predicate (IO a)
returns Predicate{..} =
  Predicate
    { predicateFunc = \io ->
        -- don't add 'setNested'; it's technically nested,
        -- but this predicate is supposed to be transparent
        io >>= predicateFunc
    , predicateDisp = predicateDisp
    , predicateDispNeg = predicateDispNeg
    }

{----- Snapshot -----}

-- TODO: --update to update snapshots
-- TODO: if no filters were added, error if outdated snapshot files (--update to remove)
-- TODO: if all tests in file were run, error if snapshot file contains outdated tests (--update to remove)
matchesSnapshot :: Typeable a => Predicate a
matchesSnapshot =
  Predicate
    { predicateFunc = \actual -> do
        testInfo <- getTestInfo
        SnapshotFixture{snapshotIndexRef} <- getFixture
        snapshotIndex <- atomicModifyIORef snapshotIndexRef $ \i -> (i + 1, i)
        let ctx =
              SnapshotContext
                { snapshotRenderers = customRenderers <> defaultSnapshotRenderers
                , snapshotTestInfo = testInfo
                , snapshotIndex
                }
        result <- checkSnapshot ctx actual
        pure
          PredicateFuncResult
            { predicateSuccess = result == SnapshotMatches
            , predicateFailMsg =
                case result of
                  SnapshotMissing -> "Snapshot does not exist. Update snapshot with --update."
                  SnapshotMatches -> "Matches snapshot"
                  SnapshotDiff snapshot renderedActual ->
                    Text.intercalate "\n" $
                      [ "Result differed from snapshot. Update snapshot with --update."
                      , showLineDiff ("expected", snapshot) ("actual", renderedActual)
                      ]
            , predicatePassMsg = "matches snapshot"
            , predicateNested = False
            }
    , predicateDisp = "matches snapshot"
    , predicateDispNeg = "does not match snapshot"
    }
  where
    -- TODO: load from SkeletestOptions
    customRenderers = []

{----- Utilities -----}

mkPredicateOp ::
  Text -- ^ operator
  -> Text -- ^ negative operator
  -> (a -> a -> Bool) -- ^ actual -> expected -> success
  -> a -- ^ expected
  -> Predicate a
mkPredicateOp op negOp f expected =
  Predicate
    { predicateFunc = \actual ->
        pure
          PredicateFuncResult
            { predicateSuccess = f actual expected
            , predicateFailMsg = explainOp (Just actual) negOp
            , predicatePassMsg = explainOp (Just actual) op
            , predicateNested = False
            }
    , predicateDisp = explainOp Nothing op
    , predicateDispNeg = explainOp Nothing negOp
    }
  where
    explainOp mActual op' =
      case mActual of
        Just actual -> render actual <> " " <> op' <> " " <> render expected
        Nothing -> op' <> " " <> render expected

render :: a -> Text
render = Text.pack . anythingToString

-- | Add parentheses if the given input contains spaces.
parens :: Text -> Text
parens s =
  if " " `Text.isInfixOf` s
    then "(" <> s <> ")"
    else s
