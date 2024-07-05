# Skeletest

Skeletest is a batteries-included test framework heavily inspired by [pytest](https://pytest.org) and [jest](https://jestjs.io). It's the built-in test framework for [Skelly](https://github.com/brandonchinn178/skelly), but it can be used as a standalone library as well.

Features:
* Seamless experience writing unit tests, property tests, and snapshot tests
* Descriptive failure messages
* Automatic fixtures management
* Rich plugin + hooks functionality

## Example

```haskell
import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range

spec :: Spec
spec = do
  describe "myFunc" $ do
    it "returns the correct list" $
      myFunc 1 2 `shouldBe` ["a", "b", "c"]

    it "returns a list containing an element" $
      myFunc 1 2 `shouldSatisfy` P.any (P.eq "a")

    it "returns a list matching the given predicates" $
      myFunc 1 2 `shouldSatisfy` P.list [P.eq "a", P.anything, P.anything]

    prop "myFunc 0 x == []" $ do
      x <- gen $ Gen.int $ Range.between (0, 100)
      myFunc 0 x `shouldBe` ""

    prop "myFunc x y == myFunc y x" $ do
      x <- gen $ Gen.int $ Range.between (0, 100)
      y <- gen $ Gen.int $ Range.between (0, 100)
      myFunc x y `shouldBe` myFunc y x

  -- top-level property that's not grouped under
  -- either myFunc nor otherFunc
  prop "myFunc x . otherFunc === id" $ do
    input <-
      gen $
        Gen.list (Range.between (0, 10)) $
          Gen.string (Range.between (0, 100) Gen.anyChar
    (myFunc x . otherFunc) input `shouldBe` id input

  describe "ioFunc" $ do
    it "returns the correct string" $ do
      DbConnFixture conn <- getFixture
      ioFunc conn 1 `shouldSatisfy` P.returns (P.eq "hello world")

    it "errors on bad input" $ do
      DbConnFixture conn <- getFixture
      ioFunc conn (-1) `shouldSatisfy` P.throws (P.eq MyException)

    it "returns the expected result" $ do
      DbConnFixture conn <- getFixture
      ioFunc conn 100 `shouldSatisfy` P.matchesSnapshot

  describe "getUser" $ do
    it "returns a matching user" $ do
      getUser "user1" `shouldSatisfy` P.con User{name = P.eq "user1", email = P.contains "@"}

newtype DbConnFixture = DbConnFixture Connection

instance Fixture DbConnFixture where
  fixtureDef =
    FixtureDef
      { fixtureScope = PerTest
      , fixtureImpl = do
          conn <- initDBConn
          setupTestTables conn
          pure . withCleanup (DbConnFixture conn) $ do
            destroyTestTables conn
            closeConn conn
      }
```

## Quickstart

TODO

## Guide

<!-- TODO: Move to website? -->

### Defining tests

TODO - describe, it, xfail, skip

### Unit tests

TODO

### Property tests

TODO

### Snapshot tests

TODO

### Fixtures

TODO

### Markers

TODO
