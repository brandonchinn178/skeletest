{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skeletest.Internal.TestRunner (
  -- * Testable
  Testable (..),

  -- * TestResult
  TestResult (..),
  TestResultMessage (..),
  testResultPass,
  testResultFromAssertionFail,
  testResultFromError,

  -- * AssertionFail
  AssertionFail (..),
  FailContext,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Stack (CallStack)
import GHC.Stack qualified as GHC
import UnliftIO.Exception (
  Exception,
  SomeException,
  displayException,
  fromException,
  try,
 )

import Skeletest.Internal.Error (SkeletestError)
import Skeletest.Internal.TestInfo (TestInfo)
import Skeletest.Internal.Utils.Color qualified as Color

{----- Testable -----}

class (MonadIO m) => Testable m where
  runTestable :: m () -> IO ()
  context :: String -> m a -> m a
  throwFailure :: AssertionFail -> m a

{----- TestResult -----}

data TestResult = TestResult
  { testResultSuccess :: Bool
  , testResultLabel :: Text
  , testResultMessage :: TestResultMessage
  }

data TestResultMessage
  = TestResultMessageNone
  | TestResultMessageInline Text
  | TestResultMessageSection Text

testResultPass :: TestResult
testResultPass =
  TestResult
    { testResultSuccess = True
    , testResultLabel = Color.green "OK"
    , testResultMessage = TestResultMessageNone
    }

testResultFromAssertionFail :: AssertionFail -> IO TestResult
testResultFromAssertionFail e = do
  msg <- renderAssertionFail e
  pure
    TestResult
      { testResultSuccess = False
      , testResultLabel = Color.red "FAIL"
      , testResultMessage = TestResultMessageSection msg
      }

testResultFromError :: SomeException -> TestResult
testResultFromError e =
  TestResult
    { testResultSuccess = False
    , testResultLabel = Color.red "ERROR"
    , testResultMessage = TestResultMessageInline $ Text.pack msg
    }
  where
    msg =
      case fromException e of
        -- In GHC 9.10+, SomeException shows the callstack, which we don't
        -- want to see for known Skeletest errors
        Just (err :: SkeletestError) -> displayException err
        Nothing -> displayException e

{----- AssertionFail -----}

data AssertionFail = AssertionFail
  { testInfo :: TestInfo
  , testFailMessage :: Text
  , testFailContext :: FailContext
  , callStack :: CallStack
  }
  deriving (Show)

instance Exception AssertionFail

-- | Context for failures, in order of most recently added -> least recently added
type FailContext = [Text]

-- | Render a test failure like:
--
-- @
-- At test/Skeletest/Internal/TestTargetsSpec.hs:19:
-- |
-- |           parseTestTargets input `shouldBe` Right (Just expected)
-- |                                   ^^^^^^^^
--
-- Right 1 ≠ Left 1
-- @
renderAssertionFail :: AssertionFail -> IO Text
renderAssertionFail AssertionFail{..} = do
  prettyStackTrace <- mapM renderCallLine . reverse $ GHC.getCallStack callStack
  pure . Text.intercalate "\n\n" . concat $
    [ prettyStackTrace
    , if null testFailContext
        then []
        else [Text.intercalate "\n" $ reverse testFailContext]
    , [testFailMessage]
    ]
  where
    renderCallLine (_, loc) = do
      let
        path = GHC.srcLocFile loc
        lineNum = GHC.srcLocStartLine loc
        startCol = GHC.srcLocStartCol loc
        endCol = GHC.srcLocEndCol loc

      mLine <-
        try (Text.readFile path) >>= \case
          Right srcFile -> pure $ getLineNum lineNum srcFile
          Left (_ :: SomeException) -> pure Nothing
      let (srcLine, pointerLine) =
            case mLine of
              Just line ->
                ( line
                , Text.replicate (startCol - 1) " " <> Text.replicate (endCol - startCol) "^"
                )
              Nothing ->
                ( "<unknown line>"
                , ""
                )

      pure . Text.intercalate "\n" $
        [ Text.pack path <> ":" <> (Text.pack . show) lineNum <> ":"
        , "|"
        , "| " <> srcLine
        , "| " <> pointerLine
        ]

    getLineNum n = listToMaybe . take 1 . drop (n - 1) . Text.lines
