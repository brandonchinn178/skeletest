# test/Skeletest/Internal/CaptureSpec.hs

## stdout ≫ is hidden on test success

```
./ExampleSpec.hs
    before: OK
    test: OK

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔
```

## stdout ≫ is rendered on test success with --format=verbose

```
./ExampleSpec.hs
╭── before: OK (0.00s)
│
╞═══ Captured stdout
│ before
╰───────────────────────────────────────────────────────────────────────────────
╭── test: OK (0.00s)
│
╞═══ Captured stdout
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔
```

## stdout ≫ is rendered on test failure

```
./ExampleSpec.hs
    before: OK
╭── test: FAIL
│ ./ExampleSpec.hs:14:
│ │
│ │     1 `shouldBe` 2
│ │       ^^^^^^^^^^
│ 
│ 1 ≠ 2
│
╞═══ Captured stdout
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 1 test passed ✔
  • 1 test failed ✘
```

## stdout ≫ is rendered on test error

```
./ExampleSpec.hs
    before: OK
╭── test: ERROR
│ ExampleSpec.hs:14:
│ │
│ │     Just _ <- pure Nothing
│ │     ^^^^^^
│ 
│ Pattern match failure in 'do' block
│
╞═══ Captured stdout
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 1 test passed ✔
  • 1 test failed ✘
```

## stdout ≫ is not captured with --capture-output=off

```
>>> stdout

./ExampleSpec.hs
    before: before
OK
    test: line1
line2
OK

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔

>>> stderr
```

## stderr ≫ is hidden on test success

```
./ExampleSpec.hs
    before: OK
    test: OK

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔
```

## stderr ≫ is rendered on test success with --format=verbose

```
./ExampleSpec.hs
╭── before: OK (0.00s)
│
╞═══ Captured stderr
│ before
╰───────────────────────────────────────────────────────────────────────────────
╭── test: OK (0.00s)
│
╞═══ Captured stderr
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔
```

## stderr ≫ is rendered on test failure

```
./ExampleSpec.hs
    before: OK
╭── test: FAIL
│ ./ExampleSpec.hs:14:
│ │
│ │     1 `shouldBe` 2
│ │       ^^^^^^^^^^
│ 
│ 1 ≠ 2
│
╞═══ Captured stderr
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 1 test passed ✔
  • 1 test failed ✘
```

## stderr ≫ is rendered on test error

```
./ExampleSpec.hs
    before: OK
╭── test: ERROR
│ ExampleSpec.hs:14:
│ │
│ │     Just _ <- pure Nothing
│ │     ^^^^^^
│ 
│ Pattern match failure in 'do' block
│
╞═══ Captured stderr
│ line1
│ line2
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 1 test passed ✔
  • 1 test failed ✘
```

## stderr ≫ is not captured with --capture-output=off

```
>>> stdout

./ExampleSpec.hs
    before: OK
    test: OK

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests passed ✔

>>> stderr

before
line1
line2
```
