module Skeletest.Internal.CLISpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

-- TODO: test parsing `--long asdf`
-- TODO: test parsing `--long=asdf`
-- TODO: test parsing `-x asdf`
-- TODO: test forwarding args with `--`
-- TODO: test --help
-- TODO: test error if same flag registered twice
-- TODO: test error if required flag not set
-- TODO: test default flag defaults to value if not set
spec :: Spec
spec = do
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
