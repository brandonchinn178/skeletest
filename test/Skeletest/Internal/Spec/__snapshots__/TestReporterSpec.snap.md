# test/Skeletest/Internal/Spec/TestReporterSpec.hs

## TestResultMessageBox ≫ renders correctly with ANSI

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

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## TestResultMessageBox ≫ renders correctly with non-ANSI

```
./ExampleSpec.hs
    should fail: FAIL
╭───╯
│ ./ExampleSpec.hs:5:
│ │
│ │ spec = it "should fail" $ 1 `shouldBe` (2 :: Int)
│ │                             ^^^^^^^^^^
│ 
│ 1 ≠ 2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```
