# test/Skeletest/Internal/CLISpec.hs

## getFlag ≫ errors if flag is not registered

```
./ExampleSpec.hs
╭── should error: ERROR
│ CLI flag 'my-flag' was not registered. Did you add it to cliFlags in Main.hs?
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```
