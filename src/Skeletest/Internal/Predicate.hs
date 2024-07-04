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
  eq,
  anything,

  -- * Data types
  just,
  -- TODO: nothing,
  -- TODO: tup2,
  -- TODO: tup3,
  -- TODO: tup4,

  -- * Numeric
  approx,
  tol,
  Tolerance (..),

  -- * Combinators
  not,
  -- TODO: P.any, P.all

  -- * IO
  returns,
  -- TODO: throws

  -- * Snapshot testing
  matchesSnapshot,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Debug.RecoverRTTI (anythingToString)
import Prelude hiding (abs, not)
import Prelude qualified

import Skeletest.Internal.Snapshot (
  SnapshotResult (..),
  checkSnapshot,
  defaultSnapshotRenderers,
 )
import Skeletest.Internal.State (getTestInfo)

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

-- TODO: if rendered vals are too long, show a diff
eq :: Eq a => a -> Predicate a
eq expected =
  Predicate
    { predicateFunc = \actual ->
        pure
          PredicateFuncResult
            { predicateSuccess = actual == expected
            , predicateFailMsg = explainOp (Just actual) "≠" expected
            , predicatePassMsg = explainOp (Just actual) "=" expected
            , predicateNested = False
            }
    , predicateDisp = explainOp Nothing "=" expected
    , predicateDispNeg = explainOp Nothing "≠" expected
    }

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
approx Tolerance{..} expected =
  Predicate
    { predicateFunc = \actual ->
        pure
          PredicateFuncResult
            { predicateSuccess = Prelude.abs (actual - expected) <= tolerance
            , predicateFailMsg = explainOp (Just actual) "≉" expected
            , predicatePassMsg = explainOp (Just actual) "≈" expected
            , predicateNested = False
            }
    , predicateDisp = explainOp Nothing "≈" expected
    , predicateDispNeg = explainOp Nothing "≉" expected
    }
  where
    mRelTol = fromTol <$> rel
    absTol = fromTol abs
    tolerance =
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
        result <- checkSnapshot (customRenderers <> defaultSnapshotRenderers) testInfo actual
        pure
          PredicateFuncResult
            { predicateSuccess = result == SnapshotMatches || True -- TODO: fix failure
            , predicateFailMsg = "does not match snapshot" -- TODO: show diff
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

explainOp :: Maybe a -> Text -> a -> Text
explainOp mActual op expected =
  case mActual of
    Just actual -> render actual <> " " <> op <> " " <> render expected
    Nothing -> op <> " " <> render expected

render :: a -> Text
render = Text.pack . anythingToString
