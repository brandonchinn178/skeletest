# Skeletest.Assertions

## context / should show failure context

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:7:
|
|     1 `shouldBe` (2 :: Int)
|       ^^^^^^^^^^

hello
world

1 ≠ 2
--------------------------------------------------------------------------------
```

## failTest / should show failure

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:5:
|
| spec = it "should fail" $ failTest "error message"
|                           ^^^^^^^^

error message
--------------------------------------------------------------------------------
```

## shouldBe / should show helpful failure

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:5:
|
| spec = it "should fail" $ 1 `shouldBe` (2 :: Int)
|                             ^^^^^^^^^^

1 ≠ 2
--------------------------------------------------------------------------------
```

## shouldNotBe / should show helpful failure

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:5:
|
| spec = it "should fail" $ 1 `shouldNotBe` (1 :: Int)
|                             ^^^^^^^^^^^^^

1 = 1
Expected:
  ≠ 1
Got:
  1
--------------------------------------------------------------------------------
```

## shouldNotSatisfy / should show helpful failure

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:6:
|
| spec = it "should fail" $ 1 `shouldNotSatisfy` P.gt (0 :: Int)
|                             ^^^^^^^^^^^^^^^^^^

1 > 0
Expected:
  ≯ 0
Got:
  1
--------------------------------------------------------------------------------
```

## shouldSatisfy / should show helpful failure

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:6:
|
| spec = it "should fail" $ (-1) `shouldSatisfy` P.gt (0 :: Int)
|                                ^^^^^^^^^^^^^^^

-1 ≯ 0
--------------------------------------------------------------------------------
```

## shows backtrace of failed assertions

```
Example
    should fail: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:6:
|
| spec = it "should fail" $ expectPositive (-1)
|                           ^^^^^^^^^^^^^^

./ExampleSpec.hs:9:
|
| expectPositive = expectGT 0
|                  ^^^^^^^^

./ExampleSpec.hs:12:
|
| expectGT x actual = actual `shouldSatisfy` P.gt x
|                            ^^^^^^^^^^^^^^^

-1 ≯ 0
--------------------------------------------------------------------------------
```
