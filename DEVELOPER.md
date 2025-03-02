# Development

```shell
cabal build --ghc-options -Werror

# Note: must use the default GHC executable; integration tests
# will not work with --with-compiler set differently
cabal exec -- cabal test --test-option '*'
```

# Release

Follow these steps to release this project:

1. Create a new branch
    1. Bump version in `skeletest.cabal`
    1. Curate `CHANGELOG.md`, creating a new section for this version and
       moving everything previously in `Unreleased` into the new section
       (keeping `Unreleased` as a section)
    1. Add comments to new features indicating when it was added (e.g.
       `-- @since 2.0.0`)
    1. Run `cabal haddock` and skim through documentation

1. Create PR as usual and merge into `main`

1. Ensure your Hackage token is set in Settings > Secrets > Actions as `HACKAGE_TOKEN_<github_username>` (replace any non alphanumeric characters in username with `_`).
    * Generate a token from `https://hackage.haskell.org/user/<hackage_username>/manage`

1. Go to the GitHub Actions page, click on the "Release" workflow, and click "Run workflow" on the main branch

1. Publish the candidate: https://hackage.haskell.org/package/skeletest/candidates

1. Publish the GitHub release: https://github.com/brandonchinn178/skeletest/releases

## Docs

Hackage still builds docs with [GHC 9.6](https://github.com/haskell/hackage-server/issues/1361), which doesn't work for this project. Until that's fixed, generate docs ourselves:

```shell
cabal v2-haddock --builddir="$PWD/dist-newstyle/docs" --haddock-for-hackage --enable-doc
```

Then on Hackage, go to "edit package information", then "Manage documentation for ...", then upload the docs. `cabal upload` has a [bug](https://github.com/haskell/cabal/issues/10252).
