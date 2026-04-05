{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.CLISpec (spec) where

import Control.Monad ((>=>))
import Data.Dynamic (fromDynamic)
import Data.Map qualified as Map
import Data.Typeable (typeOf)
import Skeletest
import Skeletest.Internal.CLI
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  spec_parseCliArgsWith
  spec_getFlag

newtype MyFlag = MyFlag String
  deriving (Eq)
newtype MyFlag2 = MyFlag2 String
  deriving (Eq)

spec_parseCliArgsWith :: Spec
spec_parseCliArgsWith = do
  describe "parseCliArgsWith" $ do
    longFlagSpec
    shortFlagSpec
    optFlagSpec
    reqFlagSpec
    switchFlagSpec
    multiFlagSpec
 where
  mkFlagInfos name mShort fspec = [(name, mShort, SomeFlagSpec fspec)]

  longFlagSpec = do
    let flags =
          mkFlagInfos "foo" Nothing $
            RequiredFlag
              { parse = pure . MyFlag
              }
    describe "long flag" $ do
      it "parses" $ do
        parseCliArgsWith flags ["--foo", "1"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "1")}
      it "parses with equal sign" $ do
        parseCliArgsWith flags ["--foo=1"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "1")}
      it "parses argument containing equal sign" $ do
        parseCliArgsWith flags ["--foo=1=2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "1=2")}
      it "errors if unknown" $ do
        parseCliArgsWith [] ["--foo"]
          `shouldSatisfy` parseFailure "Unknown flag: --foo"

  shortFlagSpec = do
    let flags =
          mkFlagInfos "foo" (Just 'f') $
            RequiredFlag
              { parse = pure . MyFlag
              }
    describe "short flag" $ do
      it "parses" $ do
        parseCliArgsWith flags ["-f", "123"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "123")}
      it "parses multiple switches at once" $ do
        let flags' =
              [ ("flag-a", Just 'a', SomeFlagSpec $ SwitchFlag (MyFlag . show))
              , ("flag-b", Just 'b', SomeFlagSpec $ SwitchFlag (MyFlag2 . show))
              ]
            expected =
              P.and
                [ containsFlag (MyFlag "True")
                , containsFlag (MyFlag2 "True")
                ]
        parseCliArgsWith flags' ["-ab"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = expected}
      it "parses short flag with arg without space" $ do
        let flags' =
              mkFlagInfos "foo" (Just 'f') $
                RequiredFlag
                  { parse = pure . MyFlag
                  }
        parseCliArgsWith flags' ["-fasdf"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "asdf")}
      it "errors if unknown" $ do
        parseCliArgsWith [] ["-x"]
          `shouldSatisfy` parseFailure "Unknown flag: -x"

  optFlagSpec = do
    let flags =
          mkFlagInfos "foo" Nothing $
            OptionalFlag
              { default_ = MyFlag ""
              , parse = pure . MyFlag
              }
    describe "OptionalFlag" $ do
      it "returns last flag" $ do
        parseCliArgsWith flags ["--foo", "1", "--foo", "2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "2")}
      it "returns default if not set" $ do
        parseCliArgsWith flags []
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "")}
      it "errors if no argument" $ do
        parseCliArgsWith flags ["--foo"]
          `shouldSatisfy` parseFailure "Flag '--foo' requires argument"

  reqFlagSpec = do
    let flags =
          mkFlagInfos "foo" Nothing $
            RequiredFlag
              { parse = pure . MyFlag
              }
    describe "RequiredFlag" $ do
      it "returns last flag" $ do
        parseCliArgsWith flags ["--foo", "1", "--foo", "2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "2")}
      it "errors if not set" $ do
        parseCliArgsWith flags []
          `shouldSatisfy` parseFailure "Flag '--foo' is required"
      it "errors if no argument" $ do
        parseCliArgsWith flags ["--foo"]
          `shouldSatisfy` parseFailure "Flag '--foo' requires argument"

  switchFlagSpec = do
    let flags =
          mkFlagInfos "foo" Nothing $
            SwitchFlag
              { fromBool = MyFlag . show
              }
    describe "SwitchFlag" $ do
      it "returns True if set" $ do
        parseCliArgsWith flags ["--foo"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "True")}
      it "returns True if set any number of times" $ do
        parseCliArgsWith flags ["--foo", "--foo", "--foo"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "True")}
      it "returns False if not set" $ do
        parseCliArgsWith flags []
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "False")}
      it "errors if argument is set" $ do
        parseCliArgsWith flags ["--foo=asdf"]
          `shouldSatisfy` parseFailure "Flag '--foo' does not take arguments, got: asdf"

  multiFlagSpec = do
    let mkMultiFlags :: (Show a) => FlagType a -> FlagInfos
        mkMultiFlags type_ =
          mkFlagInfos "foo" (Just 'f') $
            MultiFlag
              { type_
              , parseMulti = pure . MyFlag . show
              }
    describe "MultiFlag" $ do
      let flags = mkMultiFlags FlagType_Arg
      it "parses none" $ do
        parseCliArgsWith flags []
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[]")}
      it "parses one" $ do
        parseCliArgsWith flags ["--foo", "1"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[\"1\"]")}
      it "parses multiple" $ do
        parseCliArgsWith flags ["--foo", "1", "--foo", "2"]
          `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[\"1\",\"2\"]")}

      describe "FlagType_Switch" $ do
        let switchFlags = mkMultiFlags FlagType_Switch
        it "parses" $ do
          parseCliArgsWith switchFlags ["--foo", "--foo"]
            `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[True,True]")}
        it "parses multiple short flags" $ do
          parseCliArgsWith switchFlags ["-fff"]
            `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[True,True,True]")}
        it "errors if argument provided" $ do
          parseCliArgsWith switchFlags ["--foo=asdf"]
            `shouldSatisfy` parseFailure "Flag '--foo' does not take arguments, got: asdf"

      describe "FlagType_Arg" $ do
        let argFlags = mkMultiFlags FlagType_Arg
        it "parses" $ do
          parseCliArgsWith argFlags ["--foo", "1", "--foo", "2"]
            `shouldSatisfy` P.con CLIParseSuccess{flagStore = containsFlag (MyFlag "[\"1\",\"2\"]")}
        it "errors if no argument" $ do
          parseCliArgsWith argFlags ["--foo"]
            `shouldSatisfy` parseFailure "Flag '--foo' requires argument"

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
