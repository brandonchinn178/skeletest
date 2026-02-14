{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Prop.Internal (
  Property,
  PropertyM,
  runProperty,

  -- * Test
  forAll,
  discard,

  -- * Configuring properties
  setDiscardLimit,
  setShrinkLimit,
  setShrinkRetries,
  setConfidence,
  setVerifiedTermination,
  setTestLimit,

  -- * Coverage
  classify,
  cover,
  label,
  collect,

  -- * CLI flags
  PropSeedFlag,
  PropLimitFlag,
) where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Reader qualified as Trans
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import GHC.Stack qualified as GHC
import Hedgehog qualified
import Hedgehog.Internal.Property qualified as Hedgehog
import Hedgehog.Internal.Report qualified as Hedgehog hiding (defaultConfig)
import Hedgehog.Internal.Runner qualified as Hedgehog
import Hedgehog.Internal.Seed qualified as Hedgehog.Seed
import Hedgehog.Internal.Source qualified as Hedgehog
import Skeletest.Internal.CLI (FlagSpec (..), IsFlag (..), getFlag)
import Skeletest.Internal.TestInfo (TestInfo, getTestInfo)
import Skeletest.Internal.TestRunner (
  AssertionFail (..),
  FailContext,
  TestResult (..),
  TestResultMessage (..),
  Testable (..),
  testResultPass,
 )
import Skeletest.Internal.Utils.Color qualified as Color
import Text.Read (readEither, readMaybe)
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

-- | A property to run, with optional configuration settings specified up front.
--
-- Settings should be specified before any other 'Property' calls; any settings
-- specified afterwards are ignored.
type Property = PropertyM ()

data PropertyM a
  = PropertyPure [PropertyConfig] a
  | PropertyIO [PropertyConfig] (PropertyIO a)

type FailureRef = IORef (Maybe AssertionFail)
type PropertyIO a = Trans.ReaderT FailureRef (Hedgehog.PropertyT IO) a

instance Functor PropertyM where
  fmap f = \case
    PropertyPure cfg a -> PropertyPure cfg (f a)
    PropertyIO cfg m -> PropertyIO cfg (f <$> m)
instance Applicative PropertyM where
  pure = PropertyPure []
  (<*>) = ap
instance Monad PropertyM where
  PropertyPure cfg1 a >>= k =
    case k a of
      PropertyPure cfg2 b -> PropertyPure (cfg1 <> cfg2) b
      PropertyIO cfg2 m -> PropertyIO (cfg1 <> cfg2) m
  PropertyIO cfg1 fa >>= k =
    PropertyIO cfg1 $ do
      a <- fa
      case k a of
        PropertyPure _ b -> pure b
        PropertyIO _ mb -> mb
instance MonadIO PropertyM where
  liftIO = PropertyIO [] . liftIO

instance Testable PropertyM where
  runTestable = runProperty
  context msg m = PropertyIO [] (Hedgehog.annotate msg) >> m
  throwFailure e = PropertyIO [] $ do
    failureRef <- Trans.ask
    writeIORef failureRef (Just e)
    Trans.lift Hedgehog.failure

propConfig :: PropertyConfig -> Property
propConfig cfg = PropertyPure [cfg] ()

propM :: Hedgehog.PropertyT IO a -> PropertyM a
propM = PropertyIO [] . Trans.lift

data PropertyConfig
  = DiscardLimit Int
  | ShrinkLimit Int
  | ShrinkRetries Int
  | SetConfidence Int
  | SetVerifiedTermination
  | SetTestLimit Int

resolveConfig :: [PropertyConfig] -> Hedgehog.PropertyConfig
resolveConfig = foldl' go defaultConfig
 where
  defaultConfig =
    Hedgehog.PropertyConfig
      { propertyDiscardLimit = 100
      , propertyShrinkLimit = 1000
      , propertyShrinkRetries = 0
      , propertyTerminationCriteria = Hedgehog.NoConfidenceTermination 100
      , propertySkip = Nothing
      }

  go cfg = \case
    DiscardLimit x -> cfg{Hedgehog.propertyDiscardLimit = Hedgehog.DiscardLimit x}
    ShrinkLimit x -> cfg{Hedgehog.propertyShrinkLimit = Hedgehog.ShrinkLimit x}
    ShrinkRetries x -> cfg{Hedgehog.propertyShrinkRetries = Hedgehog.ShrinkRetries x}
    SetConfidence x ->
      cfg
        { Hedgehog.propertyTerminationCriteria =
            case Hedgehog.propertyTerminationCriteria cfg of
              Hedgehog.NoEarlyTermination _ tests -> Hedgehog.NoEarlyTermination (Hedgehog.Confidence $ fromIntegral x) tests
              Hedgehog.NoConfidenceTermination tests -> Hedgehog.NoEarlyTermination (Hedgehog.Confidence $ fromIntegral x) tests
              Hedgehog.EarlyTermination _ tests -> Hedgehog.EarlyTermination (Hedgehog.Confidence $ fromIntegral x) tests
        }
    SetVerifiedTermination ->
      cfg
        { Hedgehog.propertyTerminationCriteria =
            case Hedgehog.propertyTerminationCriteria cfg of
              Hedgehog.NoEarlyTermination c tests -> Hedgehog.EarlyTermination c tests
              Hedgehog.NoConfidenceTermination tests -> Hedgehog.EarlyTermination Hedgehog.defaultConfidence tests
              Hedgehog.EarlyTermination c tests -> Hedgehog.EarlyTermination c tests
        }
    SetTestLimit x ->
      cfg
        { Hedgehog.propertyTerminationCriteria =
            case Hedgehog.propertyTerminationCriteria cfg of
              Hedgehog.NoEarlyTermination c _ -> Hedgehog.NoEarlyTermination c (Hedgehog.TestLimit x)
              Hedgehog.NoConfidenceTermination _ -> Hedgehog.NoConfidenceTermination (Hedgehog.TestLimit x)
              Hedgehog.EarlyTermination c _ -> Hedgehog.EarlyTermination c (Hedgehog.TestLimit x)
        }

runProperty :: Property -> IO TestResult
runProperty = \case
  PropertyPure cfg () -> runProperty $ PropertyIO cfg (pure ())
  PropertyIO cfg m -> do
    (seed, extraConfig) <- loadPropFlags
    let cfg' = resolveConfig $ cfg <> extraConfig
    (prop, getException) <- fromPropertyIO m
    report <- Hedgehog.checkReport cfg' size seed prop reportProgress

    testInfo <- getTestInfo
    case Hedgehog.reportStatus report of
      Hedgehog.OK -> pure $ toTestResultPass report
      Hedgehog.GaveUp -> throwIO $ toGaveUpFailure testInfo report
      Hedgehog.Failed failureReport -> do
        getException >>= \case
          Nothing -> throwIO $ toAssertionFail testInfo failureReport
          Just failure -> do
            let info = getExtraTestContext report failureReport
            -- N.B. testFailContext is reversed!
            throwIO failure{testFailContext = failure.testFailContext <> reverse info}
 where
  size = 0
  reportProgress _ = pure ()

loadPropFlags :: IO (Hedgehog.Seed, [PropertyConfig])
loadPropFlags = do
  PropSeedFlag mSeed <- getFlag
  seed <- maybe Hedgehog.Seed.random pure mSeed

  PropLimitFlag mLimit <- getFlag

  let extraConfig =
        [ SetTestLimit <$> mLimit
        ]
  pure (seed, catMaybes extraConfig)

fromPropertyIO :: PropertyIO a -> IO (Hedgehog.PropertyT IO a, IO (Maybe AssertionFail))
fromPropertyIO m = do
  failureRef <- newIORef Nothing
  let
    run = Trans.runReaderT m failureRef
    getException = readIORef failureRef
  pure (run, getException)

toTestResultPass :: Hedgehog.Report Hedgehog.Result -> TestResult
toTestResultPass report =
  testResultPass
    { testResultMessage =
        TestResultMessageInline . Color.gray . Text.pack . List.intercalate "\n" . concat $
          [ [show testCount <> " tests, " <> show discards <> " discards"]
          , renderCoverage report.reportCoverage testCount
          ]
    }
 where
  Hedgehog.TestCount testCount = report.reportTests
  Hedgehog.DiscardCount discards = report.reportDiscards

toGaveUpFailure :: TestInfo -> Hedgehog.Report Hedgehog.Result -> AssertionFail
toGaveUpFailure testInfo report =
  AssertionFail
    { testInfo
    , testFailMessage =
        Text.pack . List.intercalate "\n" $
          [ "Gave up after " <> show discards <> " discards."
          , "Passed " <> show testCount <> " tests."
          ]
    , testFailContext = []
    , callStack = GHC.fromCallSiteList []
    }
 where
  Hedgehog.TestCount testCount = report.reportTests
  Hedgehog.DiscardCount discards = report.reportDiscards

toAssertionFail :: TestInfo -> Hedgehog.FailureReport -> AssertionFail
toAssertionFail testInfo Hedgehog.FailureReport{..} =
  AssertionFail
    { testInfo
    , testFailMessage = Text.pack failureMessage
    , testFailContext = []
    , callStack = toCallStack failureLocation
    }
 where
  toCallStack mSpan =
    GHC.fromCallSiteList $
      case mSpan of
        Nothing -> []
        Just Hedgehog.Span{..} ->
          let loc =
                GHC.SrcLoc
                  { srcLocPackage = ""
                  , srcLocModule = ""
                  , srcLocFile = spanFile
                  , srcLocStartLine = Hedgehog.unLineNo spanStartLine
                  , srcLocStartCol = Hedgehog.unColumnNo spanStartColumn
                  , srcLocEndLine = Hedgehog.unLineNo spanEndLine
                  , srcLocEndCol = Hedgehog.unColumnNo spanEndColumn
                  }
           in [("<unknown>", loc)]

getExtraTestContext :: Hedgehog.Report Hedgehog.Result -> Hedgehog.FailureReport -> FailContext
getExtraTestContext report Hedgehog.FailureReport{..} =
  map Text.pack . concat $
    [
      [ "Failed after " <> show testCount <> " tests."
      , "Rerun with --seed=" <> seed <> " to reproduce."
      , ""
      ]
    , [ let loc =
              case failedSpan of
                Just Hedgehog.Span{..} ->
                  List.intercalate ":" $
                    [ spanFile
                    , show . Hedgehog.unLineNo $ spanStartLine
                    , show . Hedgehog.unColumnNo $ spanStartColumn
                    ]
                Nothing -> "<unknown loc>"
         in loc <> " ==> " <> failedValue
      | Hedgehog.FailedAnnotation{..} <- failureAnnotations
      ]
    ]
 where
  Hedgehog.TestCount testCount = report.reportTests
  seed =
    let Hedgehog.Seed value gamma = report.reportSeed
     in show value <> ":" <> show gamma

renderCoverage :: Hedgehog.Coverage Hedgehog.CoverCount -> Int -> [String]
renderCoverage (Hedgehog.Coverage coverage) testCount =
  let columns =
        [ (name, percentStr, percentBar)
        | Hedgehog.MkLabel{..} <- List.sortOn Hedgehog.labelLocation $ Map.elems coverage
        , let
            Hedgehog.LabelName name = labelName
            Hedgehog.CoverCount count = labelAnnotation
            percent = round $ fromIntegral count / fromIntegral testCount * (100 :: Double)
            percentStr = show percent <> "%"
            percentBar = renderPercentBar percent
        ]
      (maxNameLen, maxPercentLen) =
        foldr
          ( \(name, percent, _) (nameAcc, percentAcc) ->
              (max (length name) nameAcc, max (length percent) percentAcc)
          )
          (0, 0)
          columns
   in [ rjust maxNameLen name <> " " <> rjust maxPercentLen percentStr <> " " <> percentBar
      | (name, percentStr, percentBar) <- columns
      ]
 where
  rjust n s = replicate (n - length s) ' ' <> s
  renderPercentBar percent =
    -- render percentage bar 20 characters wide
    let (n, r) = percent `divMod` 5
     in concat
          [ replicate n '█'
          , case r of
              0 -> ""
              1 -> "▏"
              2 -> "▍"
              3 -> "▌"
              4 -> "▊"
              _ -> "" -- unreachable
          , replicate (20 - n - (if r == 0 then 0 else 1)) '·'
          ]

{----- Test -----}

forAll :: (GHC.HasCallStack, Show a) => Hedgehog.Gen a -> PropertyM a
forAll gen = GHC.withFrozenCallStack $ propM (Hedgehog.forAll gen)

discard :: PropertyM a
discard = propM Hedgehog.discard

{----- Configuring properties -----}

setDiscardLimit :: Int -> Property
setDiscardLimit = propConfig . DiscardLimit

setShrinkLimit :: Int -> Property
setShrinkLimit = propConfig . ShrinkLimit

setShrinkRetries :: Int -> Property
setShrinkRetries = propConfig . ShrinkRetries

setConfidence :: Int -> Property
setConfidence = propConfig . SetConfidence

setVerifiedTermination :: Property
setVerifiedTermination = propConfig SetVerifiedTermination

setTestLimit :: Int -> Property
setTestLimit = propConfig . SetTestLimit

{----- Coverage -----}

-- | Record the propotion of tests which satisfy a given condition
--
-- @
-- xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int (Range.linear 0 100)
-- for_ xs $ \x -> do
--   classify "newborns" $ x == 0
--   classify "children" $ x > 0 && x < 13
--   classify "teens" $ x > 12 && x < 20
-- @
classify :: (GHC.HasCallStack) => String -> Bool -> Property
classify l cond = GHC.withFrozenCallStack $ propM $ Hedgehog.classify (Hedgehog.LabelName l) cond

-- | Require a certain percentage of the tests to be covered by the classifier.
--
-- In the following example, if the condition does not have at least 30%
-- coverage, the test will fail.
--
-- @
-- match <- forAll Gen.bool
-- cover 30 "true" $ match
-- cover 30 "false" $ not match
-- @
cover :: (GHC.HasCallStack) => Double -> String -> Bool -> Property
cover p l cond = GHC.withFrozenCallStack $ propM $ Hedgehog.cover (Hedgehog.CoverPercentage p) (Hedgehog.LabelName l) cond

-- | Add a label for each test run. It produces a table showing the percentage
-- of test runs that produced each label.
label :: (GHC.HasCallStack) => String -> Property
label l = GHC.withFrozenCallStack $ propM $ Hedgehog.label (Hedgehog.LabelName l)

-- | Like 'label', but uses 'Show' to render its argument for display.
collect :: (Show a, GHC.HasCallStack) => a -> Property
collect a = GHC.withFrozenCallStack $ propM $ Hedgehog.collect a

{----- CLI flags -----}

newtype PropSeedFlag = PropSeedFlag (Maybe Hedgehog.Seed)

instance IsFlag PropSeedFlag where
  flagName = "seed"
  flagMetaVar = "SEED"
  flagHelp = "The seed to use for property tests"
  flagSpec =
    OptionalFlag
      { flagDefault = PropSeedFlag Nothing
      , flagParse = parse
      }
   where
    parse s = maybe (Left $ "Invalid seed: " <> s) Right $ do
      (valS, ':' : gammaS) <- pure $ break (== ':') s
      val <- readMaybe valS
      gamma <- readMaybe gammaS
      pure . PropSeedFlag . Just $ Hedgehog.Seed val gamma

newtype PropLimitFlag = PropLimitFlag (Maybe Int)

instance IsFlag PropLimitFlag where
  flagName = "prop-test-limit"
  flagMetaVar = "N"
  flagHelp = "The number of tests to run per property test"
  flagSpec =
    OptionalFlag
      { flagDefault = PropLimitFlag Nothing
      , flagParse = fmap (PropLimitFlag . Just) . readEither
      }
