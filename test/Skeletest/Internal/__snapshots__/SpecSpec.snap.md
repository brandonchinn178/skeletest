# test/Skeletest/Internal/SpecSpec.hs

## focus / only runs focused test

```
./ExampleSpec.hs
    in progress: OK
```

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
