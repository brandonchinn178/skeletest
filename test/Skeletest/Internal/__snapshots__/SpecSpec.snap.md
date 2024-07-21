# Skeletest.Internal.Spec

## skip / skips tests completely

```
Example
    should not run: SKIP
        broken tests
    should not run either: SKIP
        broken tests
```

## xfail / checks for expected failures

```
Example
    should fail: XFAIL
        broken tests
    should fail too: XFAIL
        broken tests
```

## xfail / errors on unexpected passes

```
Example
    should fail: XPASS
        broken tests
    should fail too: XPASS
        broken tests
```
