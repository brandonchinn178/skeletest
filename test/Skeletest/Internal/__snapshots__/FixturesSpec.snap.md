# test/Skeletest/Internal/FixturesSpec.hs

## getFixture / detects circular dependencies

```
./ExampleSpec.hs
    should error: ERROR
╔══════════════════════════════════════════════════════════════════════════════╗
║ Found circular dependency when resolving fixtures: FixtureA -> FixtureB -> F ║
║ ixtureD -> FixtureA                                                          ║
╚══════════════════════════════════════════════════════════════════════════════╝
```
