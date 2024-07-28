module Skeletest.Internal.Testable (
  Testable (..),
  TestFailure (..),
  FailContext,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import GHC.Stack (CallStack)
import UnliftIO.Exception (Exception)

import Skeletest.Internal.TestInfo (TestInfo)

class (MonadIO m) => Testable m where
  runTestable :: m () -> IO ()
  context :: String -> m a -> m a
  throwFailure :: TestFailure -> m a

data TestFailure = TestFailure
  { testInfo :: TestInfo
  , testFailMessage :: Text
  , testFailContext :: FailContext
  , callStack :: CallStack
  }
  deriving (Show)

instance Exception TestFailure

-- | Context for failures, in order of most recently added -> least recently added
type FailContext = [Text]
