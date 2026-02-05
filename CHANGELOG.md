## Unreleased

API changes:
* Added `shouldReturn`

Runtime changes:
* Fix gray text output, using standard ANSI codes instead of a hardcoded RGB value

## v0.3.2

API changes:
* Added `hookModifyFileSpecs` to `Hooks` ([#59](https://github.com/brandonchinn178/skeletest/issues/59))

## v0.3.1

API changes:
* Re-export `BoxSpec` and `BoxContent` from `Skeletest.Plugin`

Runtime changes:
* Tweak formatting of test failures/errors

## v0.3.0

GHC support:
* Support GHC 9.14

New features:
* Automatically capture stdout/stderr ([#1](https://github.com/brandonchinn178/skeletest/issues/1))

API changes:
* Specify order of type variables for `P.anything` so that `P.anything @Int` works

Runtime changes:
* Add location to error messages
* Render test failures/errors in more visible box
* Display the path of the test file instead of guessing a module name ([#40](https://github.com/brandonchinn178/skeletest/issues/40))
* Flush stdout so test name is displayed while test is still running

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
