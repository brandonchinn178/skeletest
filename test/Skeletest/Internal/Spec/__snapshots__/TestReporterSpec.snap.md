# test/Skeletest/Internal/Spec/TestReporterSpec.hs

## Report format ≫ renders --format=minimal with ANSI

```
◈ ./ExampleSpec.hs ≫ should fail: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```

## Report format ≫ renders --format=full with ANSI

```
./ExampleSpec.hs
    should pass: OK
╭── should fail: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
    should skip: SKIP
        no run

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```

## Report format ≫ renders --format=verbose with ANSI

```
./ExampleSpec.hs
    should pass: OK (0.00s)
╭── should fail: FAIL (0.00s)
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
    should skip: SKIP (0.00s)
        no run

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```

## Report format ≫ renders --format=minimal with non-ANSI

```
◈ ./ExampleSpec.hs: 
╭── should fail: FAIL
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```

## Report format ≫ renders --format=full with non-ANSI

```
./ExampleSpec.hs
    should pass: OK
    should fail: FAIL
╭───╯
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
    should skip: SKIP
        no run

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```

## Report format ≫ renders --format=verbose with non-ANSI

```
./ExampleSpec.hs
    should pass: OK (0.00s)
    should fail: FAIL (0.00s)
╭───╯
│ ./ExampleSpec.hs:7:
│ │
│ │   it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                        ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────
    should skip: SKIP (0.00s)
        no run

═════ Test report ═════
➤ 3 tests ran in 0.00s
  • 1 test failed ✘
  • 1 test skipped ≫
```
