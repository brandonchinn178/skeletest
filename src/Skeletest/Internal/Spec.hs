module Skeletest.Internal.Spec (
  -- * Spec interface
  Spec,
  SpecTree (..),
  getSpecTrees,

  -- ** Defining a Spec
  describe,
  it,
  prop,
) where

import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Text (Text)
import Data.Text qualified as Text

import Skeletest.Prop.Internal (Property, runProperty)

type Spec = Spec' ()

newtype Spec' a = Spec (Writer [SpecTree] a)
  deriving (Functor, Applicative, Monad)

getSpecTrees :: Spec -> [SpecTree]
getSpecTrees (Spec spec) = execWriter spec

data SpecTree
  = SpecGroup Text [SpecTree]
  | SpecTest Text (IO ())

describe :: String -> Spec -> Spec
describe name spec = Spec $ tell [SpecGroup (Text.pack name) (getSpecTrees spec)]

it :: String -> IO () -> Spec
it name action = Spec $ tell [SpecTest (Text.pack name) action]

prop :: String -> Property -> Spec
prop name p = Spec $ tell [SpecTest (Text.pack name) (runProperty p)]
