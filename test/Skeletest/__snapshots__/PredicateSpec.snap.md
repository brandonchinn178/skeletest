# Skeletest.Internal.Predicate

## Combinators / && / shows helpful failure messages

```
1 ≠ 2

Expected:
  (= 2)
  and (> 0)

Got:
  1
```

```
All predicates passed

Expected:
  At least one failure:
  (= 1)
  and (> 0)

Got:
  1
```

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

## Combinators / and / shows helpful failure messages

```
1 ≠ 2

Expected:
  (= 2)
  and (> 0)
  and (< 10)

Got:
  1
```

```
All predicates passed

Expected:
  At least one failure:
  (= 1)
  and (> 0)
  and (< 10)

Got:
  1
```

## Combinators / or / shows helpful failure messages

```
No predicates passed

Expected:
  (= 2)
  or (> 1)
  or (< 0)

Got:
  1
```

```
1 > 0

Expected:
  All failures:
  (= 2)
  or (> 0)
  or (< 0)

Got:
  1
```

## Combinators / || / shows helpful failure messages

```
No predicates passed

Expected:
  (= 2)
  or (> 1)

Got:
  1
```

```
1 > 0

Expected:
  All failures:
  (= 2)
  or (> 0)

Got:
  1
```

## Containers / all / shows helpful failure messages

```
1 ≯ 10

Expected:
  all elements matching (> 10)

Got:
  [1,2]
```

```
All values matched

Expected:
  some elements not matching (> 0)

Got:
  [1,2,3]
```

## Containers / any / shows helpful failure messages

```
No values matched

Expected:
  at least one element matching (= 2)

Got:
  []
```

```
2 = 2

Expected:
  no elements matching (= 2)

Got:
  [1,2,3]
```

## Containers / elem / shows helpful failure messages

```
No values matched

Expected:
  at least one element matching (= 1)

Got:
  []
```

```
1 = 1

Expected:
  no elements matching (= 1)

Got:
  [1]
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
    • In the pattern: User x0_a2nR
      In the pattern: Just (User x0_a2nR)
      In a case alternative:
          Just (User x0_a2nR)
            -> Just
                 (Skeletest.Internal.Utils.HList.HCons
                    (pure x0_a2nR) Skeletest.Internal.Utils.HList.HNil)
  |
9 |   User "alice" (Just 1) `shouldSatisfy` P.con (User (P.eq ""))
  |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

## Data types / con / fails to compile with unknown record field

```
ExampleSpec.hs:9:43: error: [GHC-76037] Not in scope: ‘foo’
  |
9 |   User "alice" `shouldSatisfy` P.con User{foo = P.eq ""}
  |                                           ^^^
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

## Data types / tup / shows helpful failure messages

```
1 ≠ 0

Expected:
  (= 0, = [])

Got:
  (1,[])
```

```
(1 = 1, [] = [])

Expected:
  not (= 1, = [])

Got:
  (1,[])
```

## IO / returns / shows helpful failure messages

```
1 ≠ 0

Expected:
  matches Left (= 0)

Got:
  Left 1
```

```
Left (0 = 0)
```

## IO / throws / shows helpful failure messages

```
404 ≠ 500

Expected:
  throws (matches HttpException (= 500))

Got:
  HttpException 404
```

```
Expected:
  throws (matches HttpException (= 500))

Got:
  1
```

```
HttpException (404 = 404)
```

## Ord / eq / shows helpful failure messages

```
2 ≠ 1
```

```
1 = 1

Expected:
  ≠ 1

Got:
  1
```
