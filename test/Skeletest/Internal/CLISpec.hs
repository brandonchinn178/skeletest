module Skeletest.Internal.CLISpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "getFlag" $ do
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
