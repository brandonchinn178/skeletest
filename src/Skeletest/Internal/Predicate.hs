{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

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
  left,
  -- FIXME: implement
  -- nothing,
  tup,
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
  -- FIXME: P.any, P.all, P.elem, P.or
  and,

  -- * Subsequences
  HasSubsequences (..),
  hasPrefix,
  hasInfix,
  hasSuffix,

  -- * IO
  returns,
  throws,

  -- * Snapshot testing
  matchesSnapshot,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Debug.RecoverRTTI (anythingToString)
import GHC.Generics ((:*:) (..))
import UnliftIO.Exception (Exception, displayException, try)
import Prelude hiding (abs, and, not)
import Prelude qualified

import Skeletest.Internal.CLI (getFlag)
import Skeletest.Internal.Error (invariantViolation)
import Skeletest.Internal.Snapshot (
  SnapshotContext (..),
  SnapshotResult (..),
  SnapshotUpdateFlag (..),
  checkSnapshot,
  getAndIncSnapshotIndex,
  getSnapshotRenderers,
  updateSnapshot,
 )
import Skeletest.Internal.TestInfo (getTestInfo)
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
          if Prelude.not $ unTentative predicateNested
            then predicateFailMsg
            else
              Text.intercalate "\n" $
                [ predicateFailMsg
                , "Expected:"
                , indent predicateDisp
                , "Got:"
                , indent $ render val
                ]

renderPredicate :: Predicate a -> Text
renderPredicate = predicateDisp

data PredicateFuncResult = PredicateFuncResult
  { predicateSuccess :: Bool
  , predicateFailMsg :: Text
  -- ^ The message to show on failure
  , predicatePassMsg :: Text
  -- ^ The message to show on unexpected pass
  , predicateNested :: Tentative Bool
  -- ^ Did this result come from a nested predicate?
  }

data Tentative a
  = Tentative a
  | Force a

setTentative :: a -> Tentative a -> Tentative a
setTentative a = \case
  Tentative _ -> Tentative a
  Force x -> Force x

unTentative :: Tentative a -> a
unTentative = \case
  Tentative a -> a
  Force a -> a

setNested :: PredicateFuncResult -> PredicateFuncResult
setNested result = result{predicateNested = setTentative True $ predicateNested result}

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
            , predicateNested = Tentative False
            }
    , predicateDisp = "anything"
    , predicateDispNeg = "not anything"
    }

{----- Ord -----}

-- TODO: if rendered vals are too long, show a diff
eq :: (Eq a) => a -> Predicate a
eq = mkPredicateOp "=" "≠" $ \actual expected -> actual == expected

gt :: (Ord a) => a -> Predicate a
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
              , predicateNested = Tentative False
              }
    , predicateDisp = disp
    , predicateDispNeg = "not " <> disp
    }
  where
    disp = "Just (" <> predicateDisp <> ")"

left :: Predicate a -> Predicate (Either a b)
left Predicate{..} =
  Predicate
    { predicateFunc = \case
        Left a -> setNested <$> predicateFunc a
        x ->
          pure
            PredicateFuncResult
              { predicateSuccess = False
              , predicateFailMsg = render x <> " ≠ " <> disp
              , predicatePassMsg = render x <> " = " <> disp
              , predicateNested = Tentative False
              }
    , predicateDisp = disp
    , predicateDispNeg = "not " <> disp
    }
  where
    disp = "Left (" <> predicateDisp <> ")"

class IsTuple a where
  type TupleArgs a :: [Type]
  toHList :: a -> HList Identity (TupleArgs a)
instance IsTuple (a, b) where
  type TupleArgs (a, b) = '[a, b]
  toHList (a, b) = HCons (pure a) . HCons (pure b) $ HNil
instance IsTuple (a, b, c) where
  type TupleArgs (a, b, c) = '[a, b, c]
  toHList (a, b, c) = HCons (pure a) . HCons (pure b) . HCons (pure c) $ HNil
instance IsTuple (a, b, c, d) where
  type TupleArgs (a, b, c, d) = '[a, b, c, d]
  toHList (a, b, c, d) = HCons (pure a) . HCons (pure b) . HCons (pure c) . HCons (pure d) $ HNil
instance IsTuple (a, b, c, d, e) where
  type TupleArgs (a, b, c, d, e) = '[a, b, c, d, e]
  toHList (a, b, c, d, e) = HCons (pure a) . HCons (pure b) . HCons (pure c) . HCons (pure d) . HCons (pure e) $ HNil
instance IsTuple (a, b, c, d, e, f) where
  type TupleArgs (a, b, c, d, e, f) = '[a, b, c, d, e, f]
  toHList (a, b, c, d, e, f) = HCons (pure a) . HCons (pure b) . HCons (pure c) . HCons (pure d) . HCons (pure e) . HCons (pure f) $ HNil

class (IsTuple (UnPredTuple a)) => IsPredTuple a where
  type UnPredTuple a
  toHListPred :: a -> HList Predicate (TupleArgs (UnPredTuple a))
instance IsPredTuple (Predicate a, Predicate b) where
  type UnPredTuple (Predicate a, Predicate b) = (a, b)
  toHListPred (a, b) = HCons a . HCons b $ HNil
instance IsPredTuple (Predicate a, Predicate b, Predicate c) where
  type UnPredTuple (Predicate a, Predicate b, Predicate c) = (a, b, c)
  toHListPred (a, b, c) = HCons a . HCons b . HCons c $ HNil
instance IsPredTuple (Predicate a, Predicate b, Predicate c, Predicate d) where
  type UnPredTuple (Predicate a, Predicate b, Predicate c, Predicate d) = (a, b, c, d)
  toHListPred (a, b, c, d) = HCons a . HCons b . HCons c . HCons d $ HNil
instance IsPredTuple (Predicate a, Predicate b, Predicate c, Predicate d, Predicate e) where
  type UnPredTuple (Predicate a, Predicate b, Predicate c, Predicate d, Predicate e) = (a, b, c, d, e)
  toHListPred (a, b, c, d, e) = HCons a . HCons b . HCons c . HCons d . HCons e $ HNil
instance IsPredTuple (Predicate a, Predicate b, Predicate c, Predicate d, Predicate e, Predicate f) where
  type UnPredTuple (Predicate a, Predicate b, Predicate c, Predicate d, Predicate e, Predicate f) = (a, b, c, d, e, f)
  toHListPred (a, b, c, d, e, f) = HCons a . HCons b . HCons c . HCons d . HCons e . HCons f $ HNil

tup :: (IsPredTuple a) => a -> Predicate (UnPredTuple a)
tup preds =
  Predicate
    { predicateFunc = \actual -> do
        results <-
          fmap (HList.toListWith getConst) $
            HList.hzipWithM
              (\p (Identity x) -> Const <$> predicateFunc p x)
              (toHListPred preds)
              (toHList actual)
        let firstFailure = listToMaybe $ filter (Prelude.not . predicateSuccess) results
        pure
          PredicateFuncResult
            { predicateSuccess = isNothing firstFailure
            , predicateFailMsg =
                case firstFailure of
                  Just p -> predicateFailMsg p
                  -- shouldn't happen
                  Nothing -> msgNeg
            , predicatePassMsg = tupify $ map predicatePassMsg results
            , predicateNested = Tentative True
            }
    , predicateDisp = msg
    , predicateDispNeg = msgNeg
    }
  where
    tupify vals = "(" <> Text.intercalate ", " vals <> ")"
    msg = tupify $ HList.toListWith predicateDisp (toHListPred preds)
    msgNeg = "not " <> msg

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
  invariantViolation "P.con was not replaced"

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
        , predicateNested = Tentative False
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
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol 0.3
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6} 0.3
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol{P.abs = 1e-12} 0.3
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-6, P.abs = 1e-12} 0.3
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol{P.rel = Nothing} 0.3
-- >>> (0.1 + 0.2) `shouldSatisfy` P.approx P.tol{P.rel = Nothing, P.abs = 1e-12} 0.3
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
        result <- predicateFunc actual
        pure . setNested $ result{predicateSuccess = Prelude.not (predicateSuccess result)}
    , predicateDisp = predicateDispNeg
    , predicateDispNeg = predicateDisp
    }

and :: [Predicate a] -> Predicate a
and preds =
  Predicate
    { predicateFunc = \actual -> do
        results <- mapM (\p -> predicateFunc p actual) preds
        let firstFailure = listToMaybe $ filter (Prelude.not . predicateSuccess) results
        pure
          PredicateFuncResult
            { predicateSuccess = isNothing firstFailure
            , predicateFailMsg =
                case firstFailure of
                  Just p -> predicateFailMsg p
                  -- shouldn't happen
                  Nothing -> msgNeg
            , predicatePassMsg = Text.intercalate " and " $ map predicatePassMsg results
            , predicateNested = Tentative True
            }
    , predicateDisp = msg
    , predicateDispNeg = msgNeg
    }
  where
    msg = Text.intercalate " and " $ map predicateDisp preds
    msgNeg = "not " <> msg

{----- Subsequences -----}

class HasSubsequences a where
  isPrefixOf :: a -> a -> Bool
  isInfixOf :: a -> a -> Bool
  isSuffixOf :: a -> a -> Bool
instance (Eq a) => HasSubsequences [a] where
  isPrefixOf = List.isPrefixOf
  isInfixOf = List.isInfixOf
  isSuffixOf = List.isSuffixOf
instance HasSubsequences Text where
  isPrefixOf = Text.isPrefixOf
  isInfixOf = Text.isInfixOf
  isSuffixOf = Text.isSuffixOf

hasPrefix :: (HasSubsequences a) => a -> Predicate a
hasPrefix prefix =
  Predicate
    { predicateFunc = \val ->
        pure
          PredicateFuncResult
            { predicateSuccess = prefix `isPrefixOf` val
            , predicateFailMsg = render val <> " " <> msgNeg
            , predicatePassMsg = render val <> " " <> msg
            , predicateNested = Tentative False
            }
    , predicateDisp = msg
    , predicateDispNeg = msgNeg
    }
  where
    msg = "has prefix " <> render prefix
    msgNeg = "does not have prefix " <> render prefix

hasInfix :: (HasSubsequences a) => a -> Predicate a
hasInfix elems =
  Predicate
    { predicateFunc = \val ->
        pure
          PredicateFuncResult
            { predicateSuccess = elems `isInfixOf` val
            , predicateFailMsg = render val <> " " <> msgNeg
            , predicatePassMsg = render val <> " " <> msg
            , predicateNested = Tentative False
            }
    , predicateDisp = msg
    , predicateDispNeg = msgNeg
    }
  where
    msg = "has infix " <> render elems
    msgNeg = "does not have infix " <> render elems

hasSuffix :: (HasSubsequences a) => a -> Predicate a
hasSuffix suffix =
  Predicate
    { predicateFunc = \val ->
        pure
          PredicateFuncResult
            { predicateSuccess = suffix `isSuffixOf` val
            , predicateFailMsg = render val <> " " <> msgNeg
            , predicatePassMsg = render val <> " " <> msg
            , predicateNested = Tentative False
            }
    , predicateDisp = msg
    , predicateDispNeg = msgNeg
    }
  where
    msg = "has suffix " <> render suffix
    msgNeg = "does not have suffix " <> render suffix

{----- IO -----}

returns :: Predicate a -> Predicate (IO a)
returns Predicate{..} =
  Predicate
    { predicateFunc = \io -> do
        x <- io
        PredicateFuncResult{..} <- predicateFunc x
        pure
          PredicateFuncResult
            { predicateSuccess = predicateSuccess
            , predicateFailMsg =
                Text.intercalate "\n" $
                  [ predicateFailMsg
                  , "Expected:"
                  , indent predicateDisp
                  , "Got:"
                  , indent $ render x
                  ]
            , predicatePassMsg = predicatePassMsg
            , predicateNested = Force False
            }
    , predicateDisp = predicateDisp
    , predicateDispNeg = predicateDispNeg
    }

throws :: (Exception e) => Predicate e -> Predicate (IO a)
throws Predicate{..} =
  Predicate
    { predicateFunc = \io ->
        try io >>= \case
          Left e -> do
            PredicateFuncResult{..} <- predicateFunc e
            pure
              PredicateFuncResult
                { predicateSuccess = predicateSuccess
                , predicateFailMsg =
                    Text.intercalate "\n" $
                      [ predicateFailMsg
                      , "Expected:"
                      , indent disp
                      , "Got:"
                      , indent $ Text.pack $ displayException e
                      ]
                , predicatePassMsg = predicatePassMsg
                , predicateNested = Force False
                }
          Right x ->
            pure
              PredicateFuncResult
                { predicateSuccess = False
                , predicateFailMsg = render x <> " ≠ " <> disp
                , predicatePassMsg = render x <> " = " <> disp
                , predicateNested = Force False
                }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = "throws (" <> predicateDisp <> ")"
    dispNeg = "does not throw (" <> predicateDisp <> ")"

{----- Snapshot -----}

matchesSnapshot :: (Typeable a) => Predicate a
matchesSnapshot =
  Predicate
    { predicateFunc = \actual -> do
        SnapshotUpdateFlag doUpdate <- getFlag
        testInfo <- getTestInfo
        snapshotIndex <- getAndIncSnapshotIndex
        renderers <- getSnapshotRenderers
        let ctx =
              SnapshotContext
                { snapshotRenderers = renderers
                , snapshotTestInfo = testInfo
                , snapshotIndex
                }

        result <-
          if doUpdate
            then updateSnapshot ctx actual >> pure SnapshotMatches
            else checkSnapshot ctx actual

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
            , predicateNested = Tentative False
            }
    , predicateDisp = "matches snapshot"
    , predicateDispNeg = "does not match snapshot"
    }

{----- Utilities -----}

mkPredicateOp ::
  Text
  -- ^ operator
  -> Text
  -- ^ negative operator
  -> (a -> a -> Bool)
  -- ^ actual -> expected -> success
  -> a
  -- ^ expected
  -> Predicate a
mkPredicateOp op negOp f expected =
  Predicate
    { predicateFunc = \actual ->
        pure
          PredicateFuncResult
            { predicateSuccess = f actual expected
            , predicateFailMsg = explainOp (Just actual) negOp
            , predicatePassMsg = explainOp (Just actual) op
            , predicateNested = Tentative False
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

indent :: Text -> Text
indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"
