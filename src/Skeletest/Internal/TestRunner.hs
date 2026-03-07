{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Skeletest.Internal.TestRunner (
  -- * Testable
  Testable (..),

  -- * TestResult
  TestResult (..),
  TestResultMessage (..),
  testResultPass,
  testResultFromAssertionFail,
  testResultFromError,
  testResultFromErrorWith,

  -- * AssertionFail
  AssertionFail (..),
  FailContext,
) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (typeOf)
import GHC.IO.Exception qualified as GHC
import GHC.Stack (CallStack)
import GHC.Stack qualified as GHC
import Skeletest.Internal.Error (SkeletestError)
import Skeletest.Internal.Spec.Output (
  BoxSpec,
  BoxSpecContent (..),
  renderPrettyFailure,
 )
import Skeletest.Internal.TestInfo (TestInfo)
import Skeletest.Internal.Utils.Color qualified as Color
import Text.Read (readMaybe)
import UnliftIO.Exception (
  Exception,
  SomeException (..),
  displayException,
  fromException,
 )

{----- Testable -----}

class (MonadIO m) => Testable m where
  runTestable :: m () -> IO TestResult

  -- | Add any context to display if the test fails.
  --
  -- >>> (code, stdout) <- runCommand ...
  -- >>> context stdout $ code `shouldBe` ExitSuccess
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
  | TestResultMessageBox BoxSpec

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
      , testResultMessage = TestResultMessageBox [BoxText msg]
      }

testResultFromError :: SomeException -> IO TestResult
testResultFromError = testResultFromErrorWith id

testResultFromErrorWith :: (Text -> Text) -> SomeException -> IO TestResult
testResultFromErrorWith f e = do
  msg <- f <$> renderMsg
  pure
    TestResult
      { testResultSuccess = False
      , testResultLabel = Color.red "ERROR"
      , testResultMessage = TestResultMessageBox [BoxText msg]
      }
 where
  renderMsg
    | Just (err :: SkeletestError) <- fromException e = do
        pure $ Text.pack $ displayException err
    -- Handle pattern match fail in a do-block
    | Just err <- parseDoBlockFail e = do
        renderDoBlockFail err
    | SomeException err <- e = do
        pure . Text.strip . Text.pack . unlines $
          [ "Got exception of type `" <> (show . typeOf) err <> "`:"
          , displayException e
          ]

{----- DoBlockFail -----}

data DoBlockFail = DoBlockFail
  { doBlockFailMessage :: Text
  , doBlockFailFile :: FilePath
  , doBlockFailLine :: Int
  , doBlockFailStartCol :: Int
  , doBlockFailEndCol :: Int
  }

-- | See if the exception is from a pattern match fail in a do-block.
parseDoBlockFail :: SomeException -> Maybe DoBlockFail
parseDoBlockFail e = do
  GHC.IOError _ GHC.UserError _ msgStr _ _ <- fromException e
  let msg = Text.pack msgStr
  guard $ "Pattern match failure " `Text.isPrefixOf` msg
  [msgWithoutLoc, locInfo] <- pure $ Text.splitOn " at " msg
  [file, lineStr, colSpan] <- pure $ Text.splitOn ":" locInfo
  line <- readT lineStr
  [startCol, endCol] <- mapM readT $ Text.splitOn "-" colSpan
  pure
    DoBlockFail
      { doBlockFailMessage = msgWithoutLoc
      , doBlockFailFile = Text.unpack file
      , doBlockFailLine = line
      , doBlockFailStartCol = startCol
      , -- seems like srcLocEndCol is exclusive, while the columns in the fail message are inclusive
        doBlockFailEndCol = endCol + 1
      }
 where
  readT = readMaybe . Text.unpack

renderDoBlockFail :: DoBlockFail -> IO Text
renderDoBlockFail DoBlockFail{..} =
  renderPrettyFailure
    doBlockFailMessage
    doBlockFailContext
    [ (doBlockFailFile, doBlockFailLine, doBlockFailStartCol, doBlockFailEndCol)
    ]
 where
  doBlockFailContext = []

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
renderAssertionFail AssertionFail{..} =
  renderPrettyFailure
    testFailMessage
    testFailContext
    [ (srcLocFile, srcLocStartLine, srcLocStartCol, srcLocEndCol)
    | (_, GHC.SrcLoc{..}) <- GHC.getCallStack callStack
    ]
