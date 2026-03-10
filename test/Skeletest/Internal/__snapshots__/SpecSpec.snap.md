# test/Skeletest/Internal/SpecSpec.hs

## skip / skips tests completely

```
./ExampleSpec.hs
    should not run: SKIP
        broken tests
    should not run either: SKIP
        broken tests
```

## xfail / checks for expected failures

```
./ExampleSpec.hs
    should fail: XFAIL
        broken tests
    should fail too: XFAIL
        broken tests
```

## xfail / errors on unexpected passes

```
./ExampleSpec.hs
    should fail: XPASS
        broken tests
    should fail too: XPASS
        broken tests
```

## focus / only runs focused test

```
./ExampleSpec.hs
    in progress: OK
```

## focus / fails with -Werror

```
ExampleSpec.hs:6:3: error: [GHC-63394] [-Wx-focused-tests, -Werror=x-focused-tests]
    In the use of ‘focus’
    (imported from Skeletest, but defined in Skeletest.Internal.Spec.Tree):
    "focus should only be used in development"
  |
6 |   focus . it "in progress" $ pure ()
  |   ^^^^^
```
