## Unreleased

* Add location to error messages
* Render test failures/errors in more visible box
* Specify order of type variables for `P.anything` so that `P.anything @Int` works
* Display the path of the test file instead of guessing a module name ([#40](https://github.com/brandonchinn178/skeletest/issues/40))
* Flush stdout so test name is displayed while test is still running
* Support GHC 9.14
* Automatically capture stdout/stderr ([#1](https://github.com/brandonchinn178/skeletest/issues/1))

## v0.2.1

* Add `P.list`

## v0.2.0

* Move setting properties to `Skeletest.Prop`
* Add coverage functions for property tests

## v0.1.1

* Support Diff-1.0
* Support GHC 9.12, drop support for GHC 9.6

## v0.1.0

Initial release
