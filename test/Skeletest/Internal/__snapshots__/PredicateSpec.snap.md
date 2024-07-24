# Skeletest.Internal.Predicate

## Combinators / <<< / shows a helpful failure message

```
2 ≯ 10
Expected:
  > 10
Got:
  1
```

## Combinators / >>> / shows a helpful failure message

```
"1" ≠ "2"
Expected:
  = "2"
Got:
  1
```

## Data types / con / fails to compile when applied to multiple arguments

```
<no location info>: error:
    
******************** skeletest failure ********************
P.con must be applied to exactly one argument
```

## Data types / con / fails to compile when not applied to anything

```
<no location info>: error:
    
******************** skeletest failure ********************
P.con must be applied to a constructor
```

## Data types / con / fails to compile with non-constructor

```
<no location info>: error:
    
******************** skeletest failure ********************
P.con must be applied to a constructor
```

## Data types / con / fails to compile with omitted positional fields

```
ExampleSpec.hs:9:3: error: [GHC-27346]
    • The data constructor ‘User’ should have 2 arguments, but has been given 1
    • In the pattern: User x0
      In the pattern: Just (User x0)
      In a case alternative:
          Just (User x0)
            -> Just
                 ((Skeletest.Internal.Utils.HList.HCons (pure x0))
                    Skeletest.Internal.Utils.HList.HNil)
  |
9 |   User "alice" (Just 1) `shouldSatisfy` P.con (User (P.eq ""))
  |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

## Data types / con / fails to compile with unknown record field

```
ExampleSpec.hs:9:3: error: [GHC-76037] Not in scope: ‘foo’
  |
9 |   User "alice" `shouldSatisfy` P.con User{foo = P.eq ""}
  |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

## Data types / con / shows a helpful failure message

```
Example
    should error: FAIL
--------------------------------------------------------------------------------
./ExampleSpec.hs:9:
|
|   User "alice" `shouldSatisfy` P.con User{name = P.eq ""}
|                ^^^^^^^^^^^^^^^

"alice" ≠ []
Expected:
  matches User{name = (= [])}
Got:
  User "alice"
--------------------------------------------------------------------------------
```
