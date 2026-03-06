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
│ --- expected
│ +++ actual
│ @@ --0,0 +1 @@
│ +example result
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
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

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## detects corrupted snapshot files

```
./ExampleSpec.hs
╭── should error: ERROR
│ Snapshot file was corrupted: __snapshots__/ExampleSpec.snap.md
╰───────────────────────────────────────────────────────────────────────────────

╓─ 🚨 Outdated snapshots detected ────────────────
║  * __snapshots__/ExampleSpec.snap.md
║
║  Update/remove these files with --update.
╙─────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
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

## cleans up outdated snapshots

```
./Test1Spec.hs
    test other: OK
./Test2Spec.hs
    test other: OK
./Test3Spec.hs
    test: OK
    test other: OK
./Test4Spec.hs
    test: OK
./Test5Spec.hs
    test: OK

╓─ 🚨 Outdated snapshots detected ────────────────
║  * __snapshots__/Test1Spec.snap.md
║  * __snapshots__/Test2Spec.snap.md
║  * __snapshots__/Test3Spec.snap.md
║  * __snapshots__/Test4Spec.snap.md
║  * __snapshots__/Test5Spec.snap.md
║  * __snapshots__/Test6Spec.snap.md
║
║  Update/remove these files with --update.
╙─────────────────────────────────────────────────

═════ Test report ═════
➤ 6 tests ran in 0.00s
```

```
./Test1Spec.hs
    test other: OK
./Test2Spec.hs
    test other: OK
./Test3Spec.hs
    test: OK
    test other: OK
./Test4Spec.hs
    test: OK
./Test5Spec.hs
    test: OK

═════ Test report ═════
➤ 6 tests ran in 0.00s
➤ 5 snapshots updated
➤ 1 snapshot file cleaned up
```
