# Skeletest.Prop

## === / shows a helpful failure message

```
Example
    is isomorphic: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:10:
|
|     (read . show) P.=== (+ 1) `shouldSatisfy` P.isoWith (Gen.int $ Range.linear 0 10)
|                               ^^^^^^^^^^^^^^^

Failed after 1 tests.
Rerun with --seed=0:0 to reproduce.

./ExampleSpec.hs:10:47 ==> 0

0 ≠ 1
where
  0 = (read . show) 0
  1 = (+ 1) 0
--------------------------------------------------------------------------------
    is not isomorphic: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:12:
|
|     (read . show) P.=== id `shouldNotSatisfy` P.isoWith (Gen.int $ Range.linear 0 10)
|                            ^^^^^^^^^^^^^^^^^^

Failed after 1 tests.
Rerun with --seed=0:0 to reproduce.

./ExampleSpec.hs:12:47 ==> 0

0 = 0
where
  0 = (read . show) 0
  0 = id 0
--------------------------------------------------------------------------------
```

## setDiscardLimit / sets discard limit

```
Example
    discards: FAIL
--------------------------------------------------------------------------------
Gave up after 10 discards.
Passed 0 tests.
--------------------------------------------------------------------------------
```
