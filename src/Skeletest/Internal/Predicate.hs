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
  gte,
  lt,
  lte,

  -- * Data types
  just,
  nothing,
  left,
  right,
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
  (&&),
  (||),
  and,
  or,

  -- * Containers
  any,
  all,
  elem,

  -- * Subsequences
  HasSubsequences (..),
  hasPrefix,
  hasInfix,
  hasSuffix,

  -- * IO
  returns,
  throws,

  -- * Functions

  -- TODO: (encode . decode P.=== pure) `shouldSatisfy` P.isoWith (Gen.int $ Range.between 0 100)
  --   - (===) :: Eq b => (a -> b) -> (a -> b) -> Fun a b
  --   - isoWith :: MonadRunProp m => Gen a -> Predicate m (Fun a b)

  -- * Snapshot testing
  matchesSnapshot,
) where

import Data.Foldable (toList)
import Data.Foldable1 qualified as Foldable1
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Debug.RecoverRTTI (anythingToString)
import GHC.Generics ((:*:) (..))
import UnliftIO.Exception (Exception, displayException, try)
import Prelude hiding (abs, all, and, any, elem, not, or, (&&), (||))
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
        let failCtx =
              FailCtx
                { failCtxExpected = predicateDisp
                , failCtxActual = render val
                }
         in PredicateFail . withFailCtx failCtx predicateShowFailCtx $ predicateExplain

renderPredicate :: Predicate a -> Text
renderPredicate = predicateDisp

data PredicateFuncResult = PredicateFuncResult
  { predicateSuccess :: Bool
  , predicateExplain :: Text
  -- ^ The explanation of the result.
  --
  -- If predicateSuccess is true, this is the message to show if the
  -- success is unexpected. If predicatesSuccess is false, this is
  -- the message to show on the failure.
  , predicateShowFailCtx :: ShowFailCtx
  -- ^ See 'ShowFailCtx'.
  }

-- | Should a predicate show the context of the failure?
--
-- When failing `P.left (P.eq 1)`, the failure should show just the specific
-- thing that failed, e.g. `1 ≠ 2`, but we should show the general context
-- as well, e.g. `Expected: Left (= 1), Got: Left 2`. Primitive predicates
-- should generally start as NoFailCtx, then higher-order predicates should
-- upgrade it to ShowFailCtx. Predicates that explicitly want to hide the
-- context should set HideFailCtx.
data ShowFailCtx
  = NoFailCtx
  | ShowFailCtx
  | HideFailCtx
  deriving (Eq, Ord)

shouldShowFailCtx :: ShowFailCtx -> Bool
shouldShowFailCtx = \case
  NoFailCtx -> False
  ShowFailCtx -> True
  HideFailCtx -> False

data FailCtx = FailCtx
  { failCtxExpected :: Text
  , failCtxActual :: Text
  }

renderFailCtx :: FailCtx -> Text
renderFailCtx FailCtx{..} =
  Text.intercalate "\n" $
    [ "Expected:"
    , indent failCtxExpected
    , ""
    , "Got:"
    , indent failCtxActual
    ]

withFailCtx :: FailCtx -> ShowFailCtx -> Text -> Text
withFailCtx failCtx ctx s =
  if shouldShowFailCtx ctx
    then Text.intercalate "\n" [s, "", renderFailCtx failCtx]
    else s

noCtx :: ShowFailCtx
noCtx = NoFailCtx

showMergedCtxs :: [PredicateFuncResult] -> ShowFailCtx
showMergedCtxs = max ShowFailCtx . maybe NoFailCtx Foldable1.maximum . NonEmpty.nonEmpty . map predicateShowFailCtx

showCtx :: PredicateFuncResult -> PredicateFuncResult
showCtx result = result{predicateShowFailCtx = max ShowFailCtx $ predicateShowFailCtx result}

{----- General -----}

anything :: Predicate a
anything =
  Predicate
    { predicateFunc = \_ ->
        pure
          PredicateFuncResult
            { predicateSuccess = True
            , predicateExplain = "anything"
            , predicateShowFailCtx = noCtx
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

gte :: (Ord a) => a -> Predicate a
gte = mkPredicateOp "≥" "≱" $ \actual expected -> actual > expected Prelude.|| actual == expected

lt :: (Ord a) => a -> Predicate a
lt = mkPredicateOp "<" "≮" $ \actual expected -> actual < expected

lte :: (Ord a) => a -> Predicate a
lte = mkPredicateOp "≤" "≰" $ \actual expected -> actual < expected Prelude.|| actual == expected

{----- Data types -----}

just :: Predicate a -> Predicate (Maybe a)
just p = conMatches "Just" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Just x -> Just . HCons (pure x) $ HNil
      _ -> Nothing
    preds = HCons p HNil

nothing :: Predicate (Maybe a)
nothing = conMatches "Nothing" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Nothing -> Just HNil
      _ -> Nothing
    preds = HNil

left :: Predicate a -> Predicate (Either a b)
left p = conMatches "Left" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Left x -> Just . HCons (pure x) $ HNil
      _ -> Nothing
    preds = HCons p HNil

right :: Predicate b -> Predicate (Either a b)
right p = conMatches "Right" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Right x -> Just . HCons (pure x) $ HNil
      _ -> Nothing
    preds = HCons p HNil

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
    { predicateFunc = \actual ->
        verifyAll tupify <$> runPredicates (toHListPred preds) (toHList actual)
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    tupify vals = "(" <> Text.intercalate ", " vals <> ")"
    disp = tupify $ HList.toListWith predicateDisp (toHListPred preds)
    dispNeg = "not " <> disp

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
          Just fields -> verifyAll consify <$> runPredicates preds fields
          Nothing ->
            pure
              PredicateFuncResult
                { predicateSuccess = False
                , predicateExplain = render actual <> " " <> dispNeg
                , predicateShowFailCtx = noCtx
                }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    conName = Text.pack conNameS
    disp = "matches " <> predsDisp
    dispNeg = "does not match " <> predsDisp
    predsDisp = consify $ HList.toListWith predicateDisp preds

    -- consify ["= 1", "anything"] => User{id = (= 1), name = anything}
    -- consify ["= 1", "anything"] => Foo (= 1) anything
    consify vals =
      case HList.uncheck <$> mFieldNames of
        Nothing -> Text.unwords $ conName : map parens vals
        Just fieldNames ->
          let fields = zipWith (\field v -> Text.pack field <> " = " <> parens v) fieldNames vals
           in conName <> "{" <> Text.intercalate ", " fields <> "}"

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

infixr 1 <<<, >>>

(<<<) :: Predicate a -> (b -> a) -> Predicate b
Predicate{..} <<< f =
  -- TODO: render function name in predicateDisp?
  Predicate
    { predicateFunc = fmap showCtx . predicateFunc . f
    , predicateDisp
    , predicateDispNeg
    }

(>>>) :: (b -> a) -> Predicate a -> Predicate b
(>>>) = flip (<<<)

not :: Predicate a -> Predicate a
not Predicate{..} =
  Predicate
    { predicateFunc = \actual -> do
        result <- showCtx <$> predicateFunc actual
        pure result{predicateSuccess = Prelude.not $ predicateSuccess result}
    , predicateDisp = predicateDispNeg
    , predicateDispNeg = predicateDisp
    }

(&&) :: Predicate a -> Predicate a -> Predicate a
p1 && p2 = and [p1, p2]

(||) :: Predicate a -> Predicate a -> Predicate a
p1 || p2 = or [p1, p2]

and :: [Predicate a] -> Predicate a
and preds =
  Predicate
    { predicateFunc = \actual ->
        verifyAll (const "All predicates passed") <$> mapM (\p -> predicateFunc p actual) preds
    , predicateDisp = andify predList
    , predicateDispNeg = "At least one failure:\n" <> andify predList
    }
  where
    andify = Text.intercalate "\nand "
    predList = map (parens . predicateDisp) preds

or :: [Predicate a] -> Predicate a
or preds =
  Predicate
    { predicateFunc = \actual ->
        verifyAny (const "No predicates passed") <$> mapM (\p -> predicateFunc p actual) preds
    , predicateDisp = orify predList
    , predicateDispNeg = "All failures:\n" <> orify predList
    }
  where
    orify = Text.intercalate "\nor "
    predList = map (parens . predicateDisp) preds

{----- Containers -----}

any :: (Foldable t) => Predicate a -> Predicate (t a)
any Predicate{..} =
  Predicate
    { predicateFunc = \actual ->
        verifyAny (const "No values matched") <$> mapM predicateFunc (toList actual)
    , predicateDisp = "at least one element matching " <> parens predicateDisp
    , predicateDispNeg = "no elements matching " <> parens predicateDisp
    }

all :: (Foldable t) => Predicate a -> Predicate (t a)
all Predicate{..} =
  Predicate
    { predicateFunc = \actual ->
        verifyAll (const "All values matched") <$> mapM predicateFunc (toList actual)
    , predicateDisp = "all elements matching " <> parens predicateDisp
    , predicateDispNeg = "some elements not matching " <> parens predicateDisp
    }

elem :: (Eq a, Foldable t) => a -> Predicate (t a)
elem = any . eq

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
    { predicateFunc = \val -> do
        let success = prefix `isPrefixOf` val
        pure
          PredicateFuncResult
            { predicateSuccess = success
            , predicateExplain =
                if success
                  then render val <> " " <> disp
                  else render val <> " " <> dispNeg
            , predicateShowFailCtx = noCtx
            }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = "has prefix " <> render prefix
    dispNeg = "does not have prefix " <> render prefix

hasInfix :: (HasSubsequences a) => a -> Predicate a
hasInfix elems =
  Predicate
    { predicateFunc = \val -> do
        let success = elems `isInfixOf` val
        pure
          PredicateFuncResult
            { predicateSuccess = success
            , predicateExplain =
                if success
                  then render val <> " " <> disp
                  else render val <> " " <> dispNeg
            , predicateShowFailCtx = noCtx
            }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = "has infix " <> render elems
    dispNeg = "does not have infix " <> render elems

hasSuffix :: (HasSubsequences a) => a -> Predicate a
hasSuffix suffix =
  Predicate
    { predicateFunc = \val -> do
        let success = suffix `isSuffixOf` val
        pure
          PredicateFuncResult
            { predicateSuccess = success
            , predicateExplain =
                if success
                  then render val <> " " <> disp
                  else render val <> " " <> dispNeg
            , predicateShowFailCtx = noCtx
            }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = "has suffix " <> render suffix
    dispNeg = "does not have suffix " <> render suffix

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
            , predicateExplain =
                if predicateSuccess
                  then predicateExplain
                  else
                    let failCtx =
                          FailCtx
                            { failCtxExpected = predicateDisp
                            , failCtxActual = render x
                            }
                     in withFailCtx failCtx predicateShowFailCtx predicateExplain
            , predicateShowFailCtx = HideFailCtx
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
                , predicateExplain =
                    if predicateSuccess
                      then predicateExplain
                      else
                        let failCtx =
                              FailCtx
                                { failCtxExpected = disp
                                , failCtxActual = Text.pack $ displayException e
                                }
                         in withFailCtx failCtx predicateShowFailCtx predicateExplain
                , predicateShowFailCtx = HideFailCtx
                }
          Right x ->
            pure
              PredicateFuncResult
                { predicateSuccess = False
                , predicateExplain =
                    renderFailCtx
                      FailCtx
                        { failCtxExpected = disp
                        , failCtxActual = render x
                        }
                , predicateShowFailCtx = HideFailCtx
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
            , predicateExplain =
                case result of
                  SnapshotMissing -> "Snapshot does not exist. Update snapshot with --update."
                  SnapshotMatches -> "Matches snapshot"
                  SnapshotDiff snapshot renderedActual ->
                    Text.intercalate "\n" $
                      [ "Result differed from snapshot. Update snapshot with --update."
                      , showLineDiff ("expected", snapshot) ("actual", renderedActual)
                      ]
            , predicateShowFailCtx = HideFailCtx
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
    { predicateFunc = \actual -> do
        let success = f actual expected
        pure
          PredicateFuncResult
            { predicateSuccess = success
            , predicateExplain =
                if success
                  then render actual <> " " <> disp
                  else render actual <> " " <> dispNeg
            , predicateShowFailCtx = noCtx
            }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = op <> " " <> render expected
    dispNeg = negOp <> " " <> render expected

runPredicates :: HList Predicate xs -> HList Identity xs -> IO [PredicateFuncResult]
runPredicates preds = HList.toListWithM run . HList.hzip preds
  where
    run :: (Predicate :*: Identity) a -> IO PredicateFuncResult
    run (p :*: Identity x) = predicateFunc p x

verifyAll :: ([Text] -> Text) -> [PredicateFuncResult] -> PredicateFuncResult
verifyAll mergeMessages results =
  PredicateFuncResult
    { predicateSuccess = isNothing firstFailure
    , predicateExplain =
        case firstFailure of
          Just p -> predicateExplain p
          Nothing -> mergeMessages $ map predicateExplain results
    , predicateShowFailCtx = showMergedCtxs results
    }
  where
    firstFailure = listToMaybe $ filter (Prelude.not . predicateSuccess) results

verifyAny :: ([Text] -> Text) -> [PredicateFuncResult] -> PredicateFuncResult
verifyAny mergeMessages results =
  PredicateFuncResult
    { predicateSuccess = isJust firstSuccess
    , predicateExplain =
        case firstSuccess of
          Just p -> predicateExplain p
          Nothing -> mergeMessages $ map predicateExplain results
    , predicateShowFailCtx = showMergedCtxs results
    }
  where
    firstSuccess = listToMaybe $ filter predicateSuccess results

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
