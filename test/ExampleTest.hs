module ExampleTest (
  spec,
) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Skeletest
import Skeletest.Predicate qualified as P

spec :: Spec
spec = do
  describe "fixtures example" $ do
    it "allows using fixtures inside fixtures" $ do
      FixtureD <- getFixture
      TraceFixture traceRef <- getFixture
      readIORef traceRef `shouldSatisfy` P.returns ["A", "B", "C", "D"]

-- | A helper for tracing fixtures
newtype TraceFixture = TraceFixture (IORef [String])
instance Fixture TraceFixture where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTestFixture
      , fixtureImpl = do
          traceRef <- newIORef []
          pure (TraceFixture traceRef, NoCleanup)
      }

{-----
Fixtures example:
Create a diamond fixture:
  A
 / \
B   C
 \ /
  D
-----}

data FixtureA = FixtureA
instance Fixture FixtureA where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTestFixture
      , fixtureImpl = do
          TraceFixture traceRef <- getFixture
          modifyIORef' traceRef (<> ["A"])
          pure (FixtureA, NoCleanup)
      }

data FixtureB = FixtureB
instance Fixture FixtureB where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTestFixture
      , fixtureImpl = do
          FixtureA <- getFixture
          TraceFixture traceRef <- getFixture
          modifyIORef' traceRef (<> ["B"])
          pure (FixtureB, NoCleanup)
      }

data FixtureC = FixtureC
instance Fixture FixtureC where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTestFixture
      , fixtureImpl = do
          FixtureA <- getFixture
          TraceFixture traceRef <- getFixture
          modifyIORef' traceRef (<> ["C"])
          pure (FixtureC, NoCleanup)
      }

data FixtureD = FixtureD
instance Fixture FixtureD where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTestFixture
      , fixtureImpl = do
          FixtureB <- getFixture
          FixtureC <- getFixture
          TraceFixture traceRef <- getFixture
          modifyIORef' traceRef (<> ["D"])
          pure (FixtureD, NoCleanup)
      }
