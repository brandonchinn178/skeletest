# test/Skeletest/Internal/FixturesSpec.hs

## getFixture / detects circular dependencies

```
./ExampleSpec.hs
╭── should error: ERROR
│ Found circular dependency when resolving fixtures: FixtureA -> FixtureB -> FixtureD -> FixtureA
╰────────────────────────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 1 test ran in 0.00s
  • 1 test failed ✘
```

## getFixture / throws the appropriate error when setup fails

```
./ExampleSpec.hs
╭── should error: FAIL
│ ./ExampleSpec.hs:9:
│ │
│ │   fixtureAction = failTest "Fixture setup failed"
│ │                   ^^^^^^^^
│ 
│ Fixture setup failed
╰───────────────────────────────────────────────────────────────────────────────
╭── should error again: FAIL
│ ./ExampleSpec.hs:9:
│ │
│ │   fixtureAction = failTest "Fixture setup failed"
│ │                   ^^^^^^^^
│ 
│ Fixture setup failed
╰───────────────────────────────────────────────────────────────────────────────

═════ Test report ═════
➤ 2 tests ran in 0.00s
  • 2 tests failed ✘
```
