module ExampleTest (
  spec,
) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text

import Skeletest
import Skeletest.Predicate qualified as P

spec :: Spec
spec = do
  describe "predicates" $ do
    it "checks approximate equality" $ do
      let x = 0.1 + 0.2 :: Double
      x `shouldSatisfy` P.approx P.tol 0.3
      x `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-8} 0.3
      x `shouldSatisfy` P.approx P.tol{P.abs = 1e-12} 0.3
      x `shouldSatisfy` P.approx P.tol{P.rel = Just 1e-8, P.abs = 1e-12} 0.3
      x `shouldSatisfy` P.approx P.tol{P.rel = Nothing} 0.3
      x `shouldSatisfy` P.approx P.tol{P.rel = Nothing, P.abs = 1e-12} 0.3

    it "matches snapshots" $ do
      (1 :: Int) `shouldSatisfy` P.matchesSnapshot
      "a \"quoted\" string" `shouldSatisfy` P.matchesSnapshot
      Text.pack "a \"quoted\" text" `shouldSatisfy` P.matchesSnapshot

    it "matches snapshots without Show instance" $
      UserNoShow{name = "user1", age = 18} `shouldSatisfy` P.matchesSnapshot

  describe "fixtures example" $ do
    it "allows using fixtures inside fixtures" $ do
      FixtureD <- getFixture
      TraceFixture traceRef <- getFixture
      readIORef traceRef `shouldSatisfy` P.returns (P.eq ["A", "B", "C", "D"])

-- Do not add a Show instance
data UserNoShow = UserNoShow
  { name :: String
  , age :: Int
  }

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
