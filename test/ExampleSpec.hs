module ExampleSpec (
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

    it "matches constructors" $ do
      ConA `shouldSatisfy` P.con ConA
      ConB 1 2 `shouldSatisfy` P.con (ConB (P.eq 1) P.anything)

      -- record field order doesn't matter
      ConC{conC1 = 1, conC2 = 2} `shouldSatisfy` P.con ConC{conC1 = P.eq 1, conC2 = P.eq 2}
      ConC{conC1 = 1, conC2 = 2} `shouldSatisfy` P.con ConC{conC2 = P.eq 2, conC1 = P.eq 1}
      ConC{conC2 = 2, conC1 = 1} `shouldSatisfy` P.con ConC{conC1 = P.eq 1, conC2 = P.eq 2}
      ConC{conC2 = 2, conC1 = 1} `shouldSatisfy` P.con ConC{conC2 = P.eq 2, conC1 = P.eq 1}

      -- missing records always match
      ConC{conC1 = 1, conC2 = 2} `shouldSatisfy` P.con ConC{conC1 = P.eq 1}

      -- check some failures
      ConC{conC1 = 1, conC2 = 2} `shouldNotSatisfy` P.con ConA
      ConC{conC1 = 1, conC2 = 2} `shouldNotSatisfy` P.con ConC{conC1 = P.eq 123}
      ConA `shouldNotSatisfy` P.con ConC{conC1 = P.eq 2}

      -- works for type with one constructor
      UserNoShow{name = "user1", age = 18} `shouldSatisfy` P.con UserNoShow{age = P.gt 10}

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

data MyType
  = ConA
  | ConB Int Int
  | ConC { conC1 :: Int, conC2 :: Int }

-- Do not add a Show instance
data UserNoShow = UserNoShow
  { name :: String
  , age :: Int
  }

-- | A helper for tracing fixtures
newtype TraceFixture = TraceFixture (IORef [String])
instance Fixture TraceFixture where
  fixtureAction = do
    traceRef <- newIORef []
    pure . noCleanup $ TraceFixture traceRef

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
  fixtureAction = do
    TraceFixture traceRef <- getFixture
    modifyIORef' traceRef (<> ["A"])
    pure . noCleanup $ FixtureA

data FixtureB = FixtureB
instance Fixture FixtureB where
  fixtureAction = do
    FixtureA <- getFixture
    TraceFixture traceRef <- getFixture
    modifyIORef' traceRef (<> ["B"])
    pure . noCleanup $ FixtureB

data FixtureC = FixtureC
instance Fixture FixtureC where
  fixtureAction = do
    FixtureA <- getFixture
    TraceFixture traceRef <- getFixture
    modifyIORef' traceRef (<> ["C"])
    pure . noCleanup $ FixtureC

data FixtureD = FixtureD
instance Fixture FixtureD where
  fixtureAction = do
    FixtureB <- getFixture
    FixtureC <- getFixture
    TraceFixture traceRef <- getFixture
    modifyIORef' traceRef (<> ["D"])
    pure . noCleanup $ FixtureD
