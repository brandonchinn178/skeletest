# Skeletest.Internal.Snapshot

## detects corrupted snapshot files

```
Example
    should error: ERROR
        Snapshot file was corrupted: ./__snapshots__/ExampleSpec.snap.md
```

## renders JSON values

```json
{
    "hello": [
        "world",
        1
    ]
}
```

## shows helpful failure messages

```
Example
    fails: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:7:
|
|   unlines ["new1", "same1", "same2", "new2"] `shouldSatisfy` P.matchesSnapshot
|                                              ^^^^^^^^^^^^^^^

Result differed from snapshot. Update snapshot with --update.
--- expected
+++ actual
@@
+new1
 same1
-old1
 same2
-old2
+new2

--------------------------------------------------------------------------------
```
