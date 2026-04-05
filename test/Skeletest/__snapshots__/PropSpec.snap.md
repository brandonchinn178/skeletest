# test/Skeletest/PropSpec.hs

## prop ≫ shows hedgehog context for arbitrary failures

```
◈ ./ExampleSpec.hs ≫ error: ERROR
│ Got exception of type `MyException`:
│ this is MyException
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## prop ≫ renders Skeletest errors well

```
◈ ./ExampleSpec.hs ≫ error: ERROR
│ CLI flag 'my-flag' was not registered. Did you add it to cliFlags in Main.hs?
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## prop ≫ fails when configuration occurs after forAll

```
◈ ./ExampleSpec.hs ≫ discards: ERROR
│ Property configuration function must be done before any forAll or IO actions
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## prop ≫ fails when configuration occurs after IO actions

```
◈ ./ExampleSpec.hs ≫ discards: ERROR
│ Property configuration function must be done before any forAll or IO actions
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## prop ≫ supports MonadFail

```
◈ ./ExampleSpec.hs ≫ discards: ERROR
│ ExampleSpec.hs:8:
│ │
│ │   Just _ <- forAll $ pure (Nothing :: Maybe Int)
│ │   ^^^^^^
│ 
│ Pattern match failure in 'do' block
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## setDiscardLimit ≫ sets discard limit

```
./ExampleSpec.hs
╭── discards: FAIL
│ Gave up after 10 discards.
│ Passed 0 tests.
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## === ≫ shows a helpful failure message

```
◈ ./ExampleSpec.hs ≫ is isomorphic: FAIL
│ ./ExampleSpec.hs:10:
│ │
│ │     (read . show) P.=== (+ 1) `shouldSatisfy` P.isoWith (Gen.int $ Range.lin
ear 0 10)
│ │                               ^^^^^^^^^^^^^^^
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
│ 
│ ./ExampleSpec.hs:10:47 ==> 0
│ 
│ 0 ≠ 1
│ where
│   0 = (read . show) 0
│   1 = (+ 1) 0
╰───────────────────────────────────────────────────────────────────────────────
◈ ./ExampleSpec.hs ≫ is not isomorphic: FAIL
│ ./ExampleSpec.hs:12:
│ │
│ │     (read . show) P.=== id `shouldNotSatisfy` P.isoWith (Gen.int $ Range.lin
ear 0 10)
│ │                            ^^^^^^^^^^^^^^^^^^
│ 
│ Failed after 1 tests.
│ Rerun with --seed=0:0 to reproduce.
│ 
│ ./ExampleSpec.hs:12:47 ==> 0
│ 
│ 0 = 0
│ where
│   0 = (read . show) 0
│   0 = id 0
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests failed ✘
```
