{-# LANGUAGE OverloadedRecordDot #-}

module Skeletest.Internal.CaptureSpec (spec) where

import Data.List qualified as List
import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.TestUtils.Integration

spec :: Spec
spec = do
  mapM_ runtimeSpec ["stdout", "stderr"]

  describe "FixtureCapturedOutput" $ do
    mapM_ fixtureGetSpec $
      [ ("stdout", "getStdout")
      , ("stderr", "getStderr")
      ]
    mapM_ fixtureReadSpec $
      [ ("stdout", "readStdout")
      , ("stderr", "readStderr")
      ]

runtimeSpec :: String -> Spec
runtimeSpec handle = do
  describe handle $ do
    integration . it "is hidden on test success" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    " <> render_hPutStrLn handle "line1"
        , "    " <> render_hPutStrLn handle "line2"
        ]
      (code, stdout, stderr) <- runTests runner []
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot
      code `shouldBe` ExitSuccess

    integration . it "is rendered on test failure" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    " <> render_hPutStrLn handle "line1"
        , "    " <> render_hPutStrLn handle "line2"
        , "    1 `shouldBe` 2"
        ]
      (stdout, stderr) <- expectFailure $ runTests runner []
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "is rendered on test error" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    " <> render_hPutStrLn handle "line1"
        , "    " <> render_hPutStrLn handle "line2"
        , "    Just _ <- pure Nothing"
        , "    pure ()"
        ]
      (stdout, stderr) <- expectFailure $ runTests runner []
      stderr `shouldBe` ""
      stdout `shouldSatisfy` P.matchesSnapshot

    integration . it "is not captured with --capture-output=off" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    " <> render_hPutStrLn handle "line1"
        , "    " <> render_hPutStrLn handle "line2"
        ]
      (code, stdout, stderr) <- runTests runner ["--capture-output=off"]
      List.intercalate "\n\n" [">>> stdout", stdout, ">>> stderr", stderr] `shouldSatisfy` P.matchesSnapshot
      code `shouldBe` ExitSuccess

fixtureGetSpec :: (String, String) -> Spec
fixtureGetSpec (handle, func) =
  describe func $ do
    integration . it "returns captured output from current test" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    output <- getFixture @FixtureCapturedOutput"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show ""
        , "    " <> render_hPutStrLn handle "test1"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show "test1\n"
        , "    " <> render_hPutStrLn handle "test2"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show "test1\ntest2\n"
        ]
      _ <- expectSuccess $ runTests runner []
      pure ()

fixtureReadSpec :: (String, String) -> Spec
fixtureReadSpec (handle, func) =
  describe func $ do
    integration . it "returns captured output from current test" $ do
      runner <- getFixture
      addTestFile runner "ExampleSpec.hs" $
        [ "{-# LANGUAGE OverloadedRecordDot #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "module ExampleSpec (spec) where"
        , ""
        , "import Skeletest"
        , "import System.IO qualified as IO"
        , ""
        , "spec = do"
        , "  it \"before\" $ do"
        , "    " <> render_hPutStrLn handle "before"
        , "  it \"test\" $ do"
        , "    output <- getFixture @FixtureCapturedOutput"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show ""
        , "    " <> render_hPutStrLn handle "test1"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show "test1\n"
        , "    " <> render_hPutStrLn handle "test2"
        , "    s <- output." <> func
        , "    s `shouldBe` " <> show "test2\n"
        ]
      _ <- expectSuccess $ runTests runner []
      pure ()

render_hPutStrLn :: String -> String -> String
render_hPutStrLn handle s = "IO.hPutStrLn IO." <> handle <> " " <> show s
