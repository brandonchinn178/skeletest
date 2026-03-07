# test/Skeletest/Internal/SnapshotSpec.hs

## creates a new snapshot

```
./ExampleSpec.hs
╭── test: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │   "example result" `shouldSatisfy` P.matchesSnapshot
│ │                    ^^^^^^^^^^^^^^^
│ 
│ Snapshot does not exist. Update snapshot with --update.
╰───────────────────────────────────────────────────────────────────────────────
```

## detects corrupted snapshot files

```
./ExampleSpec.hs
╭── should error: ERROR
│ Snapshot file was corrupted: ./__snapshots__/ExampleSpec.snap.md
╰───────────────────────────────────────────────────────────────────────────────
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

## updates an existing snapshot

```
./ExampleSpec.hs
╭── fails: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │   unlines ["new1", "same1", "same2", "new2"] `shouldSatisfy` P.matchesSnapshot
│ │                                              ^^^^^^^^^^^^^^^
│ 
│ Result differed from snapshot. Update snapshot with --update.
│ --- expected
│ +++ actual
│ @@ -1,4 +1,4 @@
│ +new1
│  same1
│ -old1
│  same2
│ -old2
│ +new2
╰─────────────────────────────────────────────────────────────────────────────────
```
