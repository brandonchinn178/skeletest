# Skeletest

Skeletest is a batteries-included, opinionated test framework heavily inspired by [pytest](https://pytest.org) and [jest](https://jestjs.io). It's the built-in test framework for [Skelly](https://github.com/brandonchinn178/skelly), but it can be used as a standalone library as well.

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

1. If you're using Skeletest as a standalone library, add the following to your cabal file:

    ```cabal
    test-suite my-tests
      ghc-options: -F -pgmF=skeletest-preprocessor
      build-tool-depends: skeletest:skeletest-preprocessor
    ```

1. Add `Main.hs`:

    ```haskell
    import Skeletest.Main
    ```

1. To test some module `MyLib.Foo`, add a new file `MyLib/FooSpec.hs`:

    ```haskell
    module MyLib.FooSpec (spec) where

    import Skeletest
    import qualified Skeletest.Predicate as P

    spec :: Spec
    spec = do
      describe "myFunc" $ do
        it "does a thing" $ do
          myFunc 1 `shouldBe` 2
    ```

## Guide

### Defining tests

Tests should be defined in a `spec` identifier with the type `Spec`. A `Spec` is defined as a tree of tests, written using do-notation. The entire `Spec` is wrapped in an implicit `describe` containing the name of the module without the `Spec` suffix. The `describe` and `it` functions are intended to read nicely if you use types or function names as `describe` groups.

```haskell
spec :: Spec
spec = do
  -- A property test, grouped under the implicit module group.
  -- See the "Property tests" section.
  prop "encodeUser . decodeUser === id" $ do
    ...

  describe "encodeUser" $ do
    -- A unit test testing a particular aspect of the encodeUser function.
    -- See the "Unit tests" section.
    it "encodes a user with a name" $ do
      ...

    it "encodes an empty user" $ do
      ...
```

Tests can also be marked as `xfail` or `skip`. `xfail` tests will succeed if the test fails, or fail if the test unexpectedly passes. `skip` tests will skip running the test entirely. Both `xfail` and `skip` require a message explaining the reason it; this is a good place to put links to the relevant ticket or issue.

```haskell
-- xfail a single test
xfail "https://github.com/my-company/my-repo/issues/123" . it "does a thing" $ do
  ...

-- xfail multiple tests
xfail "https://github.com/my-company/my-repo/issues/123" $ do
  it "does a thing" $ do
    ...

  it "does another thing" $ do
    ...

-- skip a whole describe
skip "https://github.com/my-company/my-repo/issues/123" . describe "myFunc" $ do
  it "does a thing" $ do
    ...
```

`markManual` marks tests in the given section as manual tests, which means they won't be run when no tests are selected (see the "Test selection" section).

```haskell
markManual $ do
  ...
```

### Test selection

Test targets are specified as plain positional arguments, with the following syntax:

| Target | Explanation |
|--------|-------------|
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

### Assertions and Predicates

All assertions in Skeletest use the following functions:

* `shouldSatisfy`
* `shouldNotSatisfy` - equivalent to `shouldSatisfy` with `P.not`
* `shouldBe` - equivalent to `shouldSatisfy` with `P.eq`
* `shouldNotBe` - equivalent to `shouldNotSatisfy` with `P.eq`

`shouldSatisfy` is the most general function, but the others are provided for convenience. `shouldSatisfy` takes in the value being tested on the left, and a predicate on the right. Predicates should be imported from `Skeletest.Predicate`, qualified as `P`.

Some notable predicates are listed here. See the Haddocks for a full list of available predicates.

* `P.eq 10`
    * Satisfied when the actual value is equal to `10`.

* `P.just (P.gt 10)`
    * Satisfied when the actual value is a `Just` containing a value greater than `10`.

* `P.tup (P.eq 10, P.anything)`
    * Satisfied when the actual value is a tuple where the first element is `10` and the second element is anything. `P.tup` works for any tuple up to 6 elements.

* `P.con User{name = P.hasPrefix "user_"}`
    * Satisfied when the actual value is a `User` whose `name` field starts with `user_`. Omitted fields are not checked and can contain anything. `P.con` also works for positional constructors, except all arguments must contain predicates.

* `P.eq 10 P.<<< f`
    * Satisfied when the actual value is equal to `10` after being applied to `f`.

* `P.approx P.tol 0.5`
    * Satisfied when the actual value is approximately equal to `0.5`. Useful for floating point values. See docs for an explanation of how to adjust the tolerance with `P.tol`.

* `P.and [P.gt 0, P.lt 10]`
    * Satisfied when the actual value satisfies all of the given predicates. For just two predicates, `P.&&` can be used. Also see: `P.or`, `P.||`

* `P.returns (P.gt 10)`
    * Satisfied when the left hand side is an `IO` action that returns a value greater than `10`.

* `P.throws MyException`
    * Satisfied when the left hand side is an `IO` action that throws the given exception.

### Unit and Integration tests

Unit and integration tests are written with `it`, and run in `IO`.

```haskell
describe "mkUser" $ do
  it "creates a user with the given name" $ do
    let x = mkUser (Just "alice")
    x `shouldBe` User{name = "alice"}

    mkUser (Just "alice") `shouldBe` User{name = "alice"}

  it "creates a user with a default name" $ do
    mkUser Nothing `shouldSatisfy` P.con User{name = P.hasPrefix "user_"}

describe "addService" $ do
  it "queries the addition service" $ do
    x <- addService 1 2
    x `shouldBe` 3

    addService 1 2 `shouldSatisfy` P.returns (P.eq 3)
```

### Snapshot tests

[Snapshot tests](https://ro-che.info/articles/2017-12-04-golden-tests) can be done in any kind of test, although it's usually done in unit tests. Snapshot tests are best suited for testing that behavior doesn't change; they aren't great for testing _correctness_.

To write a snapshot test, simply use the `P.matchesSnapshot` predicate.

```haskell
myFunc 1 `shouldSatisfy` P.matchesSnapshot

-- can also snapshot within a nested predicate
fetchUserFromDb "alice" `shouldSatisfy` P.returns (P.just P.matchesSnapshot)
```

When running for the first time, or when the snapshot is changing, use the `-u`/`--update` flag. For a given test file `MyLib/FooSpec.hs`, snapshots are stored at `MyLib/__snapshots__/FooSpec.snap.md`. Snapshots are stored in a Markdown file that's easy to visually inspect in an editor or on GitHub.

Values will be rendered via their internal heap representation; even if the type has a `Show` instance, it won't be used. To use the show instance, add the following to `Main.hs`:

```haskell
snapshotRenderers =
  [ renderWithShow @User
  ]
```

You can also specify a custom renderer by implementing a `SnapshotRenderer` yourself, probably using `plainRenderer`.

Currently, old snapshots are not cleaned up, so you'll have to manually clean up snapshots if you rename or remove a test. (https://github.com/brandonchinn178/skeletest/issues/24)

### Property tests

Property tests are written with `prop` and run in the `PropertyM` monad (`Property` is an alias for `PropertyM ()`). To write property tests, add the following imports:

```haskell
import qualified Skeletest.Prop.Gen as Gen
import qualified Skeletest.Prop.Range as Range
```

Property tests consist of two things: generating random data with `forAll` and checking properties using the usual `shouldSatisfy` assertions. See the Haddocks for the different ways to generate data.

```haskell
prop "reverse does not change the length" $ do
  xs <- forAll $ Gen.list (Gen.range 0 10) Gen.int
  length (reverse xs) `shouldBe` length xs
```

One common usecase is to verify that two functions are isomorphic. This can be tested with the `P.===` operator:

```haskell
prop "decodeUser . encodeUser === pure" $ do
  let genUser = User <$> Gen.text (Gen.range 0 10) Gen.unicode
  (decodeUser . encodeUser) P.=== pure `shouldSatisfy` P.isoWith genUser
```

If a test fails, it'll say something like `Rerun with --seed=6430645105429331403:9929029875326664391 to reproduce`. Rerunning with that flag will generate the same random value for debugging.

To ignore certain values, use `discard`:

```haskell
x <- Gen.int (Gen.range (-10) 10)
when (x == 0) discard
...
```

Property tests can also be configured with the following functions. These must be called at the very beginning of the test, before any `forAll` calls. Values specified with CLI flags take precedence over the values in the code.

* `setDiscardLimit`
    * The max number of values to discard before reporting a failure
    * Default: `100`

* `setShrinkLimit`
    * The max number of shrinks before giving up
    * Default: `1000`

* `setShrinkRetries`
    * The number of times to re-run a test during shrinking. This is useful if you are testing something which fails non-deterministically and you want to increase the change of getting a good shrink. e.g. `10` means a test must pass 10 times before trying a different shrink
    * Default: `0`

* `setConfidence`
    * The acceptable occurrence of false positives. e.g. `10^9` means accepting a false positive for 1 in 10^9 tests
    * Default: don't check confidence

* `setVerifiedTermination`
    * Validate confidence is reached
    * Default: disabled

* `setTestLimit`
    * The number of tests to run before reporting success
    * Default: `100`
    * CLI flag: `--prop-test-limit`

Internally, Skeletest uses Hedgehog to run property tests, but the API is intended to stay the same, even if the underlying engine changes.

### Fixtures

Fixtures are a useful way to reuse setup logic between tests. They're commonly used to initialize a database connection, set up users, etc. Fixtures can also use other fixtures. Fixtures are cached for the given scope and cleaned up when that scope is exited.

```haskell
data DbConnFixture = DbConnFixture Connection

instance Fixture DbConnFixture where
  -- defaults to per-test
  fixtureScope = PerSessionFixture

  fixtureAction = do
    conn <- initDBConn
    pure . withCleanup (DbConnFixture conn) $ do
      closeConn conn

spec :: Spec
spec = do
  it "creates a user" $ do
    DbConnFixture conn <- getFixture
    createUser conn "alice" `shouldSatisfy` P.not (P.throws P.anything)

  it "fetches a user" $ do
    -- reuses the same connection initialized in the first test
    DbConnFixture conn <- getFixture
    getUser conn "alice" `shouldSatisfy` P.just (P.con User{name = P.eq "alice"})
```

### Markers

Markers are a useful way to mark tests for selection (see "Test selection"). There are two ways to mark a test:

1. With anonymous markers:

    ```haskell
    withMarkers ["foo", "bar"] $ do
      ...
    ```

    All tests in the given section will be marked with anonymous markers named "foo" and "bar", which can be selected with `@foo` and `@bar`, respectively.

1. With typed markers:

    ```haskell
    data MyMarker = MyMarker Int
    instance IsMarker MyMarker where
      getMarkerName _ = "my-marker"

    withMarker (MyMarker 10) $ do
      ...
    ```

    All tests in the given section will be marked with the given marker, which can be selected with `@my-marker`. You can see if a test has a marker (e.g. in `Hooks`) with `findMarkers`.

### Custom CLI flags

To register and use your own CLI flags, do the following:

1. Create an instance of `IsFlag`

1. In `Main.hs`, add the following:

    ```haskell
    import TestUtils.Flags (MyFlag)

    cliFlags =
      [ flag @MyFlag
      ]
    ```

1. In a fixture or test, do the following:

    ```haskell
    MyFlag flagVal <- getFlag
    ```

### Plugins

Skeletest is fully pluggable; any configuration specified in `Main.hs` (e.g. `cliFlags` or `snapshotRenderer`) can be defined in a `Plugin` that you can import from another module or even another package.

```haskell
module TestUtils.Plugins (myPlugin) where

import Skeletest.Plugin

myPlugin :: Plugin
myPlugin =
  defaultPlugin
    { hooks =
        defaultHooks
          { hookRunTest = \testInfo runTest -> do
              putStrLn "before test"
              result <- runTest
              putStrLn "after test"
              pure result
          }
    }
```

```haskell
import TestUtils.Plugins (myPlugin)

plugins =
  [ myPlugin
  ]
```
