{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.OptionsSpec (spec) where

import Skeletest
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  describe "main" $ do
    integration . it "works with 'MyMain'" $ do
      runner <- getFixture @TestRunner
      runner.setMainFile
        [ "module MyMain where"
        , "import Skeletest.Main"
        ]
      runner.addTestFile "ExampleSpec.hs" $
        [ "module ExampleSpec (spec) where"
        , "import Skeletest"
        , "spec = it \"should run\" $ pure ()"
        ]
      let args =
            def
              { mainFile = "MyMain.hs"
              , ghcArgs =
                  [ "-optF=main:MyMain"
                  , "-main-is"
                  , "MyMain"
                  ]
              }
      (stdout, stderr) <- expectSuccess $ runner.runTestsWith args
      stderr `shouldBe` ""
      stdout `shouldNotBe` ""
