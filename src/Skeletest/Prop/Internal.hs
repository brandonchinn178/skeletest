{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * CLI flags
  PropSeedFlag,
  PropLimitFlag,
) where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Reader qualified as Trans
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import GHC.Stack qualified as GHC
import Hedgehog qualified
import Hedgehog.Internal.Property qualified as Hedgehog
import Hedgehog.Internal.Report qualified as Hedgehog hiding (defaultConfig)
import Hedgehog.Internal.Runner qualified as Hedgehog
import Hedgehog.Internal.Seed qualified as Hedgehog.Seed
import Hedgehog.Internal.Source qualified as Hedgehog
import Text.Read (readEither, readMaybe)
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (foldl')
#endif

import Skeletest.Internal.CLI (FlagSpec (..), IsFlag (..), getFlag)
import Skeletest.Internal.TestInfo (getTestInfo)
import Skeletest.Internal.TestRunner (AssertionFail (..), Testable (..))

-- | A property to run, with optional configuration settings specified up front.
--
-- Settings should be specified before any 'forAll' or IO calls; any settings
-- specified afterwards are ignored.
type Property = PropertyM ()

data PropertyM a
  = PropertyPure [PropertyConfig] a
  | PropertyIO [PropertyConfig] (Trans.ReaderT FailureRef (Hedgehog.PropertyT IO) a)

type FailureRef = IORef (Maybe AssertionFail)

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

runProperty :: Property -> IO ()
runProperty = \case
  PropertyPure cfg () -> runProperty $ PropertyIO cfg (pure ())
  PropertyIO cfg m -> do
    failureRef <- newIORef Nothing
    (seed, extraConfig) <- loadPropFlags
    report <-
      Hedgehog.checkReport
        (resolveConfig $ cfg <> extraConfig)
        0
        seed
        (Trans.runReaderT m failureRef)
        reportProgress

    let
      Hedgehog.TestCount testCount = Hedgehog.reportTests report
      Hedgehog.DiscardCount discards = Hedgehog.reportDiscards report

    case Hedgehog.reportStatus report of
      Hedgehog.OK ->
        -- TODO: show details
        -- https://github.com/brandonchinn178/skeletest/issues/19
        pure ()
      Hedgehog.GaveUp -> do
        testInfo <- getTestInfo
        throwIO
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
      Hedgehog.Failed Hedgehog.FailureReport{..} ->
        readIORef failureRef >>= \case
          Nothing -> do
            testInfo <- getTestInfo
            throwIO
              AssertionFail
                { testInfo
                , testFailMessage = Text.pack failureMessage
                , testFailContext = []
                , callStack = toCallStack failureLocation
                }
          Just failure -> do
            let
              info =
                map Text.pack . concat $
                  [
                    [ "Failed after " <> show testCount <> " tests."
                    , "Rerun with --seed=" <> renderSeed report <> " to reproduce."
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

            throwIO
              failure
                { testFailContext =
                    -- N.B. testFailContext is reversed!
                    testFailContext failure <> reverse info
                }
  where
    reportProgress _ = pure ()
    renderSeed Hedgehog.Report{reportSeed = Hedgehog.Seed value gamma} = show value <> ":" <> show gamma
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

loadPropFlags :: IO (Hedgehog.Seed, [PropertyConfig])
loadPropFlags = do
  PropSeedFlag mSeed <- getFlag
  seed <- maybe Hedgehog.Seed.random pure mSeed

  PropLimitFlag mLimit <- getFlag

  let extraConfig =
        [ SetTestLimit <$> mLimit
        ]
  pure (seed, catMaybes extraConfig)

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
