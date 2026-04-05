# test/Skeletest/Internal/SpecSpec.hs

## skip ≫ skips tests completely

```
./ExampleSpec.hs
    should not run: SKIP
        broken tests
    should not run either: SKIP
        broken tests

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests skipped ≫
```

## xfail ≫ checks for expected failures

```
./ExampleSpec.hs
    should fail: XFAIL
        broken tests
    should fail too: XFAIL
        broken tests

═════ Test report ═════
➤ 2 tests ran in 0.00s
```

## xfail ≫ errors on unexpected passes

```
./ExampleSpec.hs
    should fail: XPASS
        broken tests
    should fail too: XPASS
        broken tests

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests failed ✘
```

## focus ≫ only runs focused test

```
./ExampleSpec.hs
    in progress: OK

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test deselected
```

## focus ≫ fails with -Werror

```
ExampleSpec.hs:6:3: error: [GHC-63394] [-Wx-focused-tests, -Werror=x-focused-tes
ts]
    In the use of ‘focus’
    (imported from Skeletest, but defined in Skeletest.Internal.Spec.Tree):
    "focus should only be used in development"
  |
6 |   focus . it "in progress" $ pure ()
  |   ^^^^^
```
