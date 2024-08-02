{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  Fun (..),
  IsoChecker (..),
  (===),
  isoWith,

  -- * Snapshot testing
  matchesSnapshot,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (toList)
import Data.Foldable1 qualified as Foldable1
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Debug.RecoverRTTI (anythingToString)
import GHC.Generics ((:*:) (..))
import GHC.Stack qualified as GHC
import UnliftIO (MonadUnliftIO)
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
import Skeletest.Prop.Gen (Gen)
import Skeletest.Prop.Internal (PropertyM, forAll)

data Predicate m a = Predicate
  { predicateFunc :: a -> m PredicateFuncResult
  , predicateDisp :: Text
  -- ^ The rendered representation of the predicate
  , predicateDispNeg :: Text
  -- ^ The rendered representation of the negation of the predicate
  }

data PredicateResult
  = PredicateSuccess
  | PredicateFail Text

runPredicate :: (Monad m) => Predicate m a -> a -> m PredicateResult
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

renderPredicate :: Predicate m a -> Text
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

anything :: (Monad m) => Predicate m a
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

eq :: (Eq a, Monad m) => a -> Predicate m a
eq = mkPredicateOp "=" "≠" $ \actual expected -> actual == expected

gt :: (Ord a, Monad m) => a -> Predicate m a
gt = mkPredicateOp ">" "≯" $ \actual expected -> actual > expected

gte :: (Ord a, Monad m) => a -> Predicate m a
gte = mkPredicateOp "≥" "≱" $ \actual expected -> actual > expected Prelude.|| actual == expected

lt :: (Ord a, Monad m) => a -> Predicate m a
lt = mkPredicateOp "<" "≮" $ \actual expected -> actual < expected

lte :: (Ord a, Monad m) => a -> Predicate m a
lte = mkPredicateOp "≤" "≰" $ \actual expected -> actual < expected Prelude.|| actual == expected

{----- Data types -----}

just :: (Monad m) => Predicate m a -> Predicate m (Maybe a)
just p = conMatches "Just" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Just x -> Just . HCons (pure x) $ HNil
      _ -> Nothing
    preds = HCons p HNil

nothing :: (Monad m) => Predicate m (Maybe a)
nothing = conMatches "Nothing" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Nothing -> Just HNil
      _ -> Nothing
    preds = HNil

left :: (Monad m) => Predicate m a -> Predicate m (Either a b)
left p = conMatches "Left" fieldNames toFields preds
  where
    fieldNames = Nothing
    toFields = \case
      Left x -> Just . HCons (pure x) $ HNil
      _ -> Nothing
    preds = HCons p HNil

right :: (Monad m) => Predicate m b -> Predicate m (Either a b)
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

class (IsTuple a) => IsPredTuple m a where
  type ToPredTuple m a
  toHListPred :: proxy a -> ToPredTuple m a -> HList (Predicate m) (TupleArgs a)
instance IsPredTuple m (a, b) where
  type ToPredTuple m (a, b) = (Predicate m a, Predicate m b)
  toHListPred _ (a, b) = HCons a . HCons b $ HNil
instance IsPredTuple m (a, b, c) where
  type ToPredTuple m (a, b, c) = (Predicate m a, Predicate m b, Predicate m c)
  toHListPred _ (a, b, c) = HCons a . HCons b . HCons c $ HNil
instance IsPredTuple m (a, b, c, d) where
  type ToPredTuple m (a, b, c, d) = (Predicate m a, Predicate m b, Predicate m c, Predicate m d)
  toHListPred _ (a, b, c, d) = HCons a . HCons b . HCons c . HCons d $ HNil
instance IsPredTuple m (a, b, c, d, e) where
  type ToPredTuple m (a, b, c, d, e) = (Predicate m a, Predicate m b, Predicate m c, Predicate m d, Predicate m e)
  toHListPred _ (a, b, c, d, e) = HCons a . HCons b . HCons c . HCons d . HCons e $ HNil
instance IsPredTuple m (a, b, c, d, e, f) where
  type ToPredTuple m (a, b, c, d, e, f) = (Predicate m a, Predicate m b, Predicate m c, Predicate m d, Predicate m e, Predicate m f)
  toHListPred _ (a, b, c, d, e, f) = HCons a . HCons b . HCons c . HCons d . HCons e . HCons f $ HNil

tup :: forall a m. (IsPredTuple m a, Monad m) => ToPredTuple m a -> Predicate m a
tup predTup =
  Predicate
    { predicateFunc = \actual ->
        verifyAll tupify <$> runPredicates preds (toHList actual)
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    preds = toHListPred (Proxy @a) predTup
    tupify vals = "(" <> Text.intercalate ", " vals <> ")"
    disp = tupify $ HList.toListWith predicateDisp preds
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
con :: a -> Predicate m a
con =
  -- A placeholder that will be replaced with conMatches in the plugin.
  invariantViolation "P.con was not replaced"

-- | A predicate for checking that a value matches the given constructor.
-- Assumes that the arguments correctly match the constructor being tested,
-- so it should not be written directly, only generated from `con`.
conMatches ::
  (Monad m) =>
  String
  -> Maybe (HList (Const String) fields)
  -> (a -> Maybe (HList Identity fields))
  -> HList (Predicate m) fields
  -> Predicate m a
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
approx :: (Fractional a, Ord a, Monad m) => Tolerance -> a -> Predicate m a
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

(<<<) :: (Monad m) => Predicate m a -> (b -> a) -> Predicate m b
Predicate{..} <<< f =
  Predicate
    { predicateFunc = fmap showCtx . predicateFunc . f
    , predicateDisp
    , predicateDispNeg
    }

(>>>) :: (Monad m) => (b -> a) -> Predicate m a -> Predicate m b
(>>>) = flip (<<<)

not :: (Monad m) => Predicate m a -> Predicate m a
not Predicate{..} =
  Predicate
    { predicateFunc = \actual -> do
        result <- showCtx <$> predicateFunc actual
        pure result{predicateSuccess = Prelude.not $ predicateSuccess result}
    , predicateDisp = predicateDispNeg
    , predicateDispNeg = predicateDisp
    }

(&&) :: (Monad m) => Predicate m a -> Predicate m a -> Predicate m a
p1 && p2 = and [p1, p2]

(||) :: (Monad m) => Predicate m a -> Predicate m a -> Predicate m a
p1 || p2 = or [p1, p2]

and :: (Monad m) => [Predicate m a] -> Predicate m a
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

or :: (Monad m) => [Predicate m a] -> Predicate m a
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

any :: (Foldable t, Monad m) => Predicate m a -> Predicate m (t a)
any Predicate{..} =
  Predicate
    { predicateFunc = \actual ->
        verifyAny (const "No values matched") <$> mapM predicateFunc (toList actual)
    , predicateDisp = "at least one element matching " <> parens predicateDisp
    , predicateDispNeg = "no elements matching " <> parens predicateDisp
    }

all :: (Foldable t, Monad m) => Predicate m a -> Predicate m (t a)
all Predicate{..} =
  Predicate
    { predicateFunc = \actual ->
        verifyAll (const "All values matched") <$> mapM predicateFunc (toList actual)
    , predicateDisp = "all elements matching " <> parens predicateDisp
    , predicateDispNeg = "some elements not matching " <> parens predicateDisp
    }

elem :: (Eq a, Foldable t, Monad m) => a -> Predicate m (t a)
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

hasPrefix :: (HasSubsequences a, Monad m) => a -> Predicate m a
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

hasInfix :: (HasSubsequences a, Monad m) => a -> Predicate m a
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

hasSuffix :: (HasSubsequences a, Monad m) => a -> Predicate m a
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

returns :: (MonadIO m) => Predicate m a -> Predicate m (m a)
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

throws :: (Exception e, MonadUnliftIO m) => Predicate m e -> Predicate m (m a)
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

{----- Functions -----}

data Fun a b = Fun String (a -> b)
data IsoChecker a b = IsoChecker (Fun a b) (Fun a b)

-- | Use in conjunction with 'isoWith' to verify if two functions are isomorphic.
(===) :: (a -> b) -> (a -> b) -> IsoChecker a b
f === g = IsoChecker (Fun "lhs" f) (Fun "rhs" g)

infix 2 ===

isoWith :: (GHC.HasCallStack, Show a, Eq b) => Gen a -> Predicate PropertyM (IsoChecker a b)
isoWith gen =
  Predicate
    { predicateFunc = \(IsoChecker (Fun f1DispS f1) (Fun f2DispS f2)) -> do
        a <- GHC.withFrozenCallStack $ forAll gen
        let
          f1Disp = Text.pack f1DispS
          f2Disp = Text.pack f2DispS
          b1 = f1 a
          b2 = f2 a
          aDisp = parens $ render a
          b1Disp = parens $ render b1
          b2Disp = parens $ render b2
        pure
          PredicateFuncResult
            { predicateSuccess = b1 == b2
            , predicateExplain =
                Text.intercalate "\n" $
                  [ b1Disp <> " " <> (if b1 == b2 then "=" else "≠") <> " " <> b2Disp
                  , "where"
                  , indent $ b1Disp <> " = " <> f1Disp <> " " <> aDisp
                  , indent $ b2Disp <> " = " <> f2Disp <> " " <> aDisp
                  ]
            , predicateShowFailCtx = HideFailCtx
            }
    , predicateDisp = disp
    , predicateDispNeg = dispNeg
    }
  where
    disp = "isomorphic"
    dispNeg = "not isomorphic"

{----- Snapshot -----}

matchesSnapshot :: (Typeable a, MonadIO m) => Predicate m a
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
  (Monad m) =>
  Text
  -- ^ operator
  -> Text
  -- ^ negative operator
  -> (a -> a -> Bool)
  -- ^ actual -> expected -> success
  -> a
  -- ^ expected
  -> Predicate m a
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

runPredicates :: (Monad m) => HList (Predicate m) xs -> HList Identity xs -> m [PredicateFuncResult]
runPredicates preds = HList.toListWithM run . HList.hzip preds
  where
    run :: (Predicate m :*: Identity) a -> m PredicateFuncResult
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
