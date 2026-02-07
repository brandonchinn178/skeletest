module Skeletest.PluginSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "runTest" $ do
    integration . it "allows hooking into test execution" $ do
      runner <- getFixture
      setMainFile runner $
        [ "import Skeletest.Main"
        , "import Skeletest.Plugin"
        , ""
        , "plugins = [defaultPlugin{hooks = myHooks}]"
        , "myHooks = defaultHooks"
        , "  { runTest = \\_ run -> do"
        , "      putStrLn \"before test\""
        , "      result <- run"
        , "      putStrLn \"after test\""
        , "      pure result"
        , "  }"
        ]
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , "import Skeletest"
        , "spec = it \"should run\" $ pure ()"
        ]
      (stdout, _) <- expectSuccess $ runTests runner []
      stdout `shouldSatisfy` P.matchesSnapshot

  describe "modifySpecRegistry" $ do
    integration . it "allows modifying specs" $ do
      runner <- getFixture
      setMainFile runner $
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
        , "  { modifySpecRegistry = \\_ modify -> (fmap . mapSpecs . mapSpecTrees) update . modify"
        , "  }"
        , " where"
        , "  update go = filter isValid . map go"
        , "  isValid = \\case"
        , "    SpecTree_Group{} -> True"
        , "    SpecTree_Test test -> not $ \"SKIP\" `T.isPrefixOf` test.name"
        ]
      addTestFile runner "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , "import Skeletest"
        , "spec = do"
        , "  it \"should run\" $ pure ()"
        , "  it \"SKIP should not run\" $ failTest \"bad\""
        ]
      (stdout, _) <- expectSuccess $ runTests runner []
      stdout `shouldSatisfy` P.matchesSnapshot
