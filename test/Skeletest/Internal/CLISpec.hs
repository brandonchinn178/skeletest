{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.CLISpec (spec) where

import Control.Monad ((>=>))
import Data.Dynamic (fromDynamic)
import Data.Map qualified as Map
import Data.Typeable (Typeable, typeOf)
import Skeletest
import Skeletest.Internal.CLI (
  CLIFlagStore,
  CLIParseResult (..),
  flag,
  parseCliArgs,
 )
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

newtype FooFlag = FooFlag String
  deriving (Eq)
instance IsFlag FooFlag where
  flagName = "foo"
  flagHelp = "test"
  flagSpec =
    OptionalFlag
      { flagDefault = FooFlag ""
      , flagParse = Right . FooFlag
      }

spec :: Spec
spec = do
  describe "parseCliArgs" $ do
    it "parses long flag" $ do
      parseCliArgs [flag @FooFlag] ["--foo", "1"]
        `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (FooFlag "1")}

    it "parses long flag with equal sign" $ do
      parseCliArgs [flag @FooFlag] ["--foo=1"]
        `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (FooFlag "1")}

    it "parses long flag containing equal sign" $ do
      parseCliArgs [flag @FooFlag] ["--foo=1=2"]
        `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (FooFlag "1=2")}

  describe "getFlag" $ do
    integration . it "reads registered flag" $ do
      runner <- getFixture @TestRunner
      runner.setMainFile
        [ "import Skeletest.Main"
        , "import ExampleSpec (MyFlag)"
        , "cliFlags = [flag @MyFlag]"
        ]
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (MyFlag, spec) where"
        , "import Skeletest"
        , ""
        , "newtype MyFlag = MyFlag String"
        , "instance IsFlag MyFlag where"
        , "  flagName = \"my-flag\""
        , "  flagHelp = \"example\""
        , "  flagSpec = RequiredFlag (Right . MyFlag)"
        , ""
        , "spec = it \"should get flag\" $ do"
        , "  MyFlag s <- getFlag"
        , "  s `shouldBe` \"hello world\""
        ]

      _ <- expectSuccess $ runner.runTestsWith def{cliArgs = ["--my-flag", "hello world"]}
      pure ()

    integration . it "errors if flag is not registered" $ do
      runner <- getFixture @TestRunner
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , ""
        , "newtype MyFlag = MyFlag Bool"
        , "instance IsFlag MyFlag where"
        , "  flagName = \"my-flag\""
        , "  flagHelp = \"example\""
        , "  flagSpec = SwitchFlag MyFlag"
        , ""
        , "spec = it \"should error\" $ do"
        , "  MyFlag _ <- getFlag"
        , "  pure ()"
        ]

      (stdout, stderr) <- expectFailure runner.runTests
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

containsFlag :: (Typeable a, Eq a) => a -> Predicate IO CLIFlagStore
containsFlag f = (Map.lookup (typeOf f) >=> fromDynamic) P.>>> P.just (P.eq f)
