# test/Skeletest/Internal/CaptureSpec.hs

## stderr / is hidden on test success

```
./ExampleSpec.hs
    before: OK
    test: OK
```

## stderr / is not captured with --capture-output=off

```
>>> stdout

./ExampleSpec.hs
    before: OK
    test: OK

>>> stderr

before
line1
line2
```

```
./ExampleSpec.hs
    before: OK
    test: OK
```

## stderr / is rendered on test error

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
```

## stderr / is rendered on test failure

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
```

## stdout / is hidden on test success

```
./ExampleSpec.hs
    before: OK
    test: OK
```

## stdout / is not captured with --capture-output=off

```
>>> stdout

./ExampleSpec.hs
    before: before
OK
    test: line1
line2
OK

>>> stderr
```

```
./ExampleSpec.hs
    before: before
OK
    test: line1
line2
OK
```

## stdout / is rendered on test error

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
```

## stdout / is rendered on test failure

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
```
