# Skeletest

Skeletest is a batteries-included test framework heavily inspired by [pytest](https://pytest.org) and [jest](https://jestjs.io). It's the built-in test framework for [Skelly](https://github.com/brandonchinn178/skelly), but it can be used as a standalone library as well.

Features:
* Seamless experience writing unit tests, property tests, and snapshot tests
* Descriptive failure messages
* Easy test selection from CLI
* Automatic fixtures management
* Rich plugin + hooks functionality

## Example

```haskell
import Skeletest
import qualified Skeletest.Predicate as P
import qualified Skeletest.Prop.Gen as Gen
import qualified Skeletest.Prop.Range as Range

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
      x <- gen $ Gen.int (Range.linear 0 100)
      myFunc 0 x `shouldBe` ""

    prop "myFunc x y == myFunc y x" $ do
      x <- gen $ Gen.int (Range.linear 0 100)
      y <- gen $ Gen.int (Range.linear 0 100)
      myFunc x y `shouldBe` myFunc y x

  -- top-level property that's not grouped under
  -- either myFunc nor otherFunc
  prop "myFunc x . otherFunc === id" $ do
    x <- gen $ Gen.int (Range.linear 0 100)
    let input = 
          Gen.list (Range.linear 0 10) $
            Gen.string (Range.linear 0 100) Gen.unicode
    (myFunc x . otherFunc) P.=== id `shouldSatisfy` P.isoWith input

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
  fixtureAction = do
    conn <- initDBConn
    setupTestTables conn
    pure . withCleanup (DbConnFixture conn) $ do
      destroyTestTables conn
      closeConn conn
```

## Quickstart

TODO

## Guide

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

### Test selection

Test targets are specified as plain positional arguments, with the following syntax:

| Target | Notes |
|--------|-------|
| `*`                     | Selects all tests (useful to include manual tests) |
| `[myFooFunc]`           | Tests including substring      |
| `@fast`                 | Tests tagged with marker       |
| `test/MyLib/FooSpec.hs` | Tests in file, relative to CWD |
| `test/MyLib/FooSpec.hs[myFooFunc]` | Syntax sugar for `(test/MyLib/FooSpec.hs and [myFooFunc])` |
| `[func1] and @fast`     | Tests matching both targets    |
| `[func1] or @fast`      | Tests matching either target   |
| `not [func1]`           | Tests not matching target      |

Some more examples:
* `test/MySpec.hs and ([myFooFunc] or [myBarFunc]) and @fast`
* `[myFooFunc] or test/MySpec.hs[myBarFunc]`
 
When multiple targets are specified, they are joined with `or`.
