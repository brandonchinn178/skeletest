{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.CLISpec (spec) where

import Control.Monad ((>=>))
import Data.Dynamic (fromDynamic)
import Data.Map qualified as Map
import Data.Typeable (typeOf)
import Skeletest
import Skeletest.Internal.CLI (
  CLIParseResult (..),
  flag,
  parseCliArgs,
 )
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

newtype ToggleFlag = ToggleFlag Bool
  deriving (Eq)
instance IsFlag ToggleFlag where
  flagName = "toggle"
  flagHelp = "test"
  flagSpec =
    SwitchFlag
      { flagFromBool = ToggleFlag
      }

newtype ReqFlag = ReqFlag String
  deriving (Eq)
instance IsFlag ReqFlag where
  flagName = "req"
  flagHelp = "test"
  flagSpec =
    RequiredFlag
      { flagParse = Right . ReqFlag
      }

newtype OptFlag = OptFlag String
  deriving (Eq)
instance IsFlag OptFlag where
  flagName = "opt"
  flagShort = Just 'o'
  flagHelp = "test"
  flagSpec =
    OptionalFlag
      { flagDefault = OptFlag ""
      , flagParse = Right . OptFlag
      }

spec :: Spec
spec = do
  spec_parseCliArgs
  spec_getFlag

spec_parseCliArgs :: Spec
spec_parseCliArgs = do
  describe "parseCliArgs" $ do
    longFlagSpec
    shortFlagSpec
    optFlagSpec
    reqFlagSpec
    switchFlagSpec
 where
  longFlagSpec = do
    describe "long flag" $ do
      it "parses" $ do
        parseCliArgs [flag @OptFlag] ["--opt", "1"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "1")}

      it "parses with equal sign" $ do
        parseCliArgs [flag @OptFlag] ["--opt=1"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "1")}

      it "parses argument containing equal sign" $ do
        parseCliArgs [flag @OptFlag] ["--opt=1=2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "1=2")}

      it "errors if unknown" $ do
        parseCliArgs [] ["--foo"]
          `shouldSatisfy` parseFailure "Unknown flag: --foo"

  shortFlagSpec = do
    describe "short flag" $ do
      it "parses" $ do
        parseCliArgs [flag @OptFlag] ["-o", "123"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "123")}

      it "errors if argument has multiple characters" $ do
        parseCliArgs [flag @OptFlag] ["-oasdf"]
          `shouldSatisfy` parseFailure "Invalid flag: -oasdf"

      it "errors if unknown" $ do
        parseCliArgs [] ["-x"]
          `shouldSatisfy` parseFailure "Unknown flag: -x"

  optFlagSpec = do
    describe "OptionalFlag" $ do
      it "stores last flag" $ do
        parseCliArgs [flag @OptFlag] ["--opt", "1", "--opt", "2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "2")}

      it "stores default if not set" $ do
        parseCliArgs [flag @OptFlag] []
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (OptFlag "")}

      it "errors if no argument" $ do
        parseCliArgs [flag @OptFlag] ["--opt"]
          `shouldSatisfy` parseFailure "Flag '--opt' requires argument"

  reqFlagSpec = do
    describe "RequiredFlag" $ do
      it "stores last flag" $ do
        parseCliArgs [flag @ReqFlag] ["--req", "1", "--req", "2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (ReqFlag "2")}

      it "errors if not set" $ do
        parseCliArgs [flag @ReqFlag] []
          `shouldSatisfy` parseFailure "Flag '--req' is required"

      it "errors if no argument" $ do
        parseCliArgs [flag @ReqFlag] ["--req"]
          `shouldSatisfy` parseFailure "Flag '--req' requires argument"

  switchFlagSpec = do
    describe "SwitchFlag" $ do
      it "stores True if set" $ do
        parseCliArgs [flag @ToggleFlag] ["--toggle"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (ToggleFlag True)}

      it "stores True if set any number of times" $ do
        parseCliArgs [flag @ToggleFlag] ["--toggle", "--toggle", "--toggle"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (ToggleFlag True)}

      it "stores False if not set" $ do
        parseCliArgs [flag @ToggleFlag] []
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (ToggleFlag False)}

      xfail "broken" . it "errors if argument is set" $ do
        parseCliArgs [flag @ToggleFlag] ["--toggle=asdf"]
          `shouldSatisfy` parseFailure "Flag '--toggle' does not take arguments, got: asdf"

  containsFlag f = (Map.lookup (typeOf f) >=> fromDynamic) P.>>> P.just (P.eq f)
  parseFailure msg = P.con (CLIParseFailure (P.eq msg))

spec_getFlag :: Spec
spec_getFlag = do
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
