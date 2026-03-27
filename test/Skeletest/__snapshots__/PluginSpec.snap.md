# test/Skeletest/PluginSpec.hs

## runTest ≫ allows hooking into test execution

```
./ExampleSpec.hs
    should run: before test
after test
OK

═════ Test report ═════
➤ 1 test ran in 0.00s
```

## modifySpecRegistry ≫ allows modifying specs

```
./ExampleSpec.hs
    should run: OK

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test deselected
```
