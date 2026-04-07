{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.PluginSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "runTest" $ do
    integration . it "allows hooking into test execution" $ do
      runner <- getFixture @TestRunner
      runner.setMainFile
        [ "import Skeletest.Main"
        , "import Skeletest.Plugin"
        , ""
        , "plugins = [defaultPlugin{hooks = myHooks}]"
        , "myHooks = defaultHooks"
        , "  { runTest = runEarly $ mkHook_"
        , "      (\\_ _ -> putStrLn \"before test\")"
        , "      (\\_ _ _ -> putStrLn \"after test\")"
        , "  }"
        ]
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , "import Skeletest"
        , "spec = it \"should run\" $ pure ()"
        ]
      (stdout, _) <- expectSuccess $ runner.runTestsWith def{simulateANSI = False}
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "modifySpecRegistry" $ do
    integration . it "allows modifying specs" $ do
      runner <- getFixture @TestRunner
      runner.setMainFile
        [ "{-# LANGUAGE DisambiguateRecordFields #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        , "{-# LANGUAGE NamedFieldPuns #-}"
        , "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "import qualified Data.Text as T"
        , "import Skeletest.Main"
        , "import Skeletest.Plugin"
        , ""
        , "plugins = [defaultPlugin{hooks = myHooks}]"
        , "myHooks = defaultHooks"
        , "  { modifySpecRegistry = mkPreHook $ \\_ -> pure . mapSpecs (filterSpecTests isValid)"
        , "  }"
        , " where"
        , "  isValid = not . (\"SKIP\" `T.isPrefixOf`) . (.name)"
        ]
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , "import Skeletest"
        , "spec = do"
        , "  it \"should run\" $ pure ()"
        , "  it \"SKIP should not run\" $ failTest \"bad\""
        ]
      (stdout, _) <- expectSuccess runner.runTests
      stdout `shouldSatisfy` P.matchesSnapshot
