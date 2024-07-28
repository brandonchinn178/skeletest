{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Prop.Internal (
  Property,
  PropertyM,
  runProperty,

  -- * Gen
  forAll,

  -- * Configuring properties
  setDiscardLimit,
  setShrinkLimit,
  setShrinkRetries,
  setConfidence,
  setVerifiedTermination,
  setTestLimit,
  setSkipTo,

  -- * CLI flags
  PropSeedFlag,
) where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Reader qualified as Trans
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Stack qualified as GHC
import Hedgehog qualified
import Hedgehog.Internal.Property qualified as Hedgehog
import Hedgehog.Internal.Report qualified as Hedgehog hiding (defaultConfig)
import Hedgehog.Internal.Runner qualified as Hedgehog
import Hedgehog.Internal.Seed qualified as Hedgehog.Seed
import Hedgehog.Internal.Source qualified as Hedgehog
import Text.Read (readMaybe)
import UnliftIO.Exception (throwIO)
import UnliftIO.IORef (IORef, newIORef, readIORef, writeIORef)

import Skeletest.Internal.CLI (FlagSpec (..), IsFlag (..), getFlag)
import Skeletest.Internal.TestInfo (getTestInfo)
import Skeletest.Internal.Testable (TestFailure (..), Testable (..))

-- | A property to run, with optional configuration settings specified up front.
--
-- Settings should be specified before any 'forAll' or IO calls; any settings
-- specified afterwards are ignored.
type Property = PropertyM ()

data PropertyM a
  = PropertyPure [PropertyConfig] a
  | PropertyIO [PropertyConfig] (Trans.ReaderT FailureRef (Hedgehog.PropertyT IO) a)

type FailureRef = IORef (Maybe TestFailure)

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
  | SkipTo String

resolveConfig :: [PropertyConfig] -> Hedgehog.PropertyConfig
resolveConfig = foldl' go Hedgehog.defaultConfig
  where
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
      SkipTo s -> cfg{Hedgehog.propertySkip = Just (fromString s)}

runProperty :: Property -> IO ()
runProperty = \case
  PropertyPure cfg () -> runProperty $ PropertyIO cfg (pure ())
  PropertyIO cfg m -> do
    failureRef <- newIORef Nothing
    seed <-
      getFlag >>= \case
        PropSeedFlag (Just seed) -> pure seed
        _ -> Hedgehog.Seed.random
    report <-
      Hedgehog.checkReport
        (resolveConfig cfg)
        0
        seed
        (Trans.runReaderT m failureRef)
        reportProgress

    let Hedgehog.TestCount testCount = Hedgehog.reportTests report
    case Hedgehog.reportStatus report of
      Hedgehog.OK -> pure () -- TODO: show (# tests ran, # discards, coverage)
      Hedgehog.GaveUp -> error "gave up" -- FIXME: throw TestFailure
      Hedgehog.Failed Hedgehog.FailureReport{..} ->
        readIORef failureRef >>= \case
          Nothing -> do
            testInfo <- getTestInfo
            throwIO
              TestFailure
                { testInfo
                , testFailMessage = Text.pack failureMessage
                , testFailContext = []
                , callStack =
                    GHC.fromCallSiteList $
                      case failureLocation of
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
                }
          Just failure -> do
            -- FIXME: add failureAnnotations + failureFootnotes?
            let msg =
                  [ "Failed after " <> show testCount <> " tests."
                  , "Rerun with --seed=" <> renderSeed report <> " to reproduce."
                  ]
            throwIO
              failure
                { testFailContext =
                    -- N.B. testFailContext is reversed!
                    testFailContext failure <> reverse (map Text.pack msg)
                }
  where
    reportProgress _ = pure () -- TODO: show progress?
    renderSeed Hedgehog.Report{reportSeed = Hedgehog.Seed value gamma} = show value <> ":" <> show gamma

{----- Gen -----}

forAll :: (Show a) => Hedgehog.Gen a -> PropertyM a
forAll = propM . Hedgehog.forAll

{----- Configuring properties -----}

-- FIXME: test with integration test
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

setSkipTo :: String -> Property
setSkipTo = propConfig . SkipTo

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
