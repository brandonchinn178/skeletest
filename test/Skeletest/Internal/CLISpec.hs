module Skeletest.Internal.CLISpec (spec) where

import Control.Monad ((>=>))
import Data.Dynamic (fromDynamic)
import Data.Map qualified as Map
import Data.Typeable (Typeable, typeOf)
import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.Internal.CLI (
  CLIFlagStore,
  CLIParseResult (..),
  flag,
  parseCliArgs,
 )
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

-- TODO: test parsing `-x asdf`
-- TODO: test forwarding args with `--`
-- TODO: test --help
-- TODO: test error if same flag registered twice
-- TODO: test error if required flag not set
-- TODO: test default flag defaults to value if not set
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
      runner <- getFixture
      setMainFile runner $
        [ "import Skeletest.Main"
        , "import ExampleSpec (MyFlag)"
        , "cliFlags = [flag @MyFlag]"
        ]
      addTestFile runner "ExampleSpec.hs" $
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

      _ <- expectSuccess $ runTests runner ["--my-flag", "hello world"]
      pure ()

    integration . it "errors if flag is not registered" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
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

      (code, stdout, stderr) <- runTests runner []
      code `shouldBe` ExitFailure 1
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

containsFlag :: (Typeable a, Eq a) => a -> Predicate CLIFlagStore
containsFlag f = (Map.lookup (typeOf f) >=> fromDynamic) P.>>> P.just (P.eq f)
