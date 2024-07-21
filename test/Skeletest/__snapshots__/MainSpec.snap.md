# Skeletest.Main

## errors if Skeletest.Main not imported

```
skeletest-preprocessor: 
******************** skeletest failure ********************
Could not find Skeletest.Main import in Main module

Main.hs:1:1: error:
    `skeletest-preprocessor' failed in phase `Haskell pre-processor'. (Exit code: 1)

*** Exception: ExitFailure 1
```

## errors if main function defined

```
<no location info>: error: [GHC-29916]
    Multiple declarations of ‘main’
    Declared at: Main.hs:4:1
                 <no location info>
```
