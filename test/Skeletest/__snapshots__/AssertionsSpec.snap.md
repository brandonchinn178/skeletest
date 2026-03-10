# test/Skeletest/AssertionsSpec.hs

## shouldBe / should show helpful failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:5:
│ │
│ │ spec = it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                             ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
```

## shouldNotBe / should show helpful failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:5:
│ │
│ │ spec = it "should fail" $ 1 `shouldNotBe` (1 :: Int)
│ │                             ^^^^^^^^^^^^^
│ 
│ 1 = 1
│ 
│ Expected:
│   ≠ 1
│ 
│ Got:
│   1
╰───────────────────────────────────────────────────────────────────────────────
```

## shouldSatisfy / should show helpful failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:6:
│ │
│ │ spec = it "should fail" $ (-1) `shouldSatisfy` P.gt (0 :: Int)
│ │                                ^^^^^^^^^^^^^^^
│ 
│ -1 ≯ 0
╰───────────────────────────────────────────────────────────────────────────────
```

## shouldNotSatisfy / should show helpful failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:6:
│ │
│ │ spec = it "should fail" $ 1 `shouldNotSatisfy` P.gt (0 :: Int)
│ │                             ^^^^^^^^^^^^^^^^^^
│ 
│ 1 > 0
│ 
│ Expected:
│   ≯ 0
│ 
│ Got:
│   1
╰───────────────────────────────────────────────────────────────────────────────
```

## shouldReturn / should show helpful failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:5:
│ │
│ │ spec = it "should fail" $ pure 1 `shouldReturn` (2 :: Int)
│ │                                  ^^^^^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
```

## context / should show failure context

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │     1 `shouldBe` (2 :: Int)
│ │       ^^^^^^^^^^
│ 
│ hello
│ world
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
```

## failTest / should show failure

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:5:
│ │
│ │ spec = it "should fail" $ failTest "error message"
│ │                           ^^^^^^^^
│ 
│ error message
╰───────────────────────────────────────────────────────────────────────────────
```

## shows backtrace of failed assertions

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:6:
│ │
│ │ spec = it "should fail" $ expectPositive (-1)
│ │                           ^^^^^^^^^^^^^^
│ 
│ ./ExampleSpec.hs:9:
│ │
│ │ expectPositive = expectGT 0
│ │                  ^^^^^^^^
│ 
│ ./ExampleSpec.hs:12:
│ │
│ │ expectGT x actual = actual `shouldSatisfy` P.gt x
│ │                            ^^^^^^^^^^^^^^^
│ 
│ -1 ≯ 0
╰───────────────────────────────────────────────────────────────────────────────
```

## shows helpful error on pattern match fail

```
./ExampleSpec.hs
╭── should fail: ERROR
│ ExampleSpec.hs:7:
│ │
│ │   Just x <- pure Nothing
│ │   ^^^^^^
│ 
│ Pattern match failure in 'do' block
╰───────────────────────────────────────────────────────────────────────────────
```

## shows unrecognized exceptions

```
./ExampleSpec.hs
╭── should fail: ERROR
│ Got exception of type `IOException`:
│ unknown-file.txt: openFile: does not exist (No such file or directory)
╰───────────────────────────────────────────────────────────────────────────────
```

## shows source code when running from different directory

```
./ExampleSpec.hs
╭── should fail: FAIL
│ ./ExampleSpec.hs:3:
│ │
│ │ spec = it "should fail" $ failTest "failure"
│ │                           ^^^^^^^^
│ 
│ failure
╰───────────────────────────────────────────────────────────────────────────────
```
