{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.TestTargetsSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.Internal.TestTargets

spec :: Spec
spec = do
  describe "matchesTest" $ do
    let someAttrs =
          TestAttrs
            { testPath = "MyTestSpec.hs"
            , testIdentifier = ["a", "b", "test name"]
            , testMarkers = ["mark1", "mark2"]
            }

    sequence_
      [ it label $
          matchesTest selection attrs `shouldBe` True
      | (label, selection, attrs) <-
          [ ( "matches tests in file"
            , TestTargetFile "FooSpec.hs"
            , someAttrs{testPath = "FooSpec.hs"}
            )
          , ( "matches test name substring"
            , TestTargetName "foo"
            , someAttrs{testIdentifier = ["group1", "group2", "my foo test"]}
            )
          , ( "matches group name substring"
            , TestTargetName "fooFunc"
            , someAttrs{testIdentifier = ["fooFunction", "does a thing"]}
            )
          , ( "matches a marker exactly"
            , TestTargetMarker "fast"
            , someAttrs{testMarkers = ["fast", "slow"]}
            )
          , ( "matches a NOT target when target does not match"
            , TestTargetNot (TestTargetFile "FooSpec.hs")
            , someAttrs{testPath = "BarSpec.hs"}
            )
          , ( "matches an AND target when target matches both"
            , TestTargetAnd (TestTargetFile "FooSpec.hs") (TestTargetMarker "fast")
            , someAttrs{testPath = "FooSpec.hs", testMarkers = ["fast"]}
            )
          , ( "matches an OR target when target matches one"
            , TestTargetOr (TestTargetFile "FooSpec.hs") (TestTargetMarker "fast")
            , someAttrs{testPath = "FooSpec.hs", testMarkers = []}
            )
          ]
      ]

    sequence_
      [ it label $
          matchesTest selection attrs `shouldBe` False
      | (label, selection, attrs) <-
          [ ( "does not match test in another file"
            , TestTargetFile "FooSpec.hs"
            , someAttrs{testPath = "BarSpec.hs"}
            )
          , ( "does not match test not containing name"
            , TestTargetName "foo"
            , someAttrs{testIdentifier = ["group1", "group2", "other test"]}
            )
          , ( "does not match marker substring"
            , TestTargetMarker "fastish"
            , someAttrs{testMarkers = ["fast"]}
            )
          , ( "does not match a NOT target when target matches"
            , TestTargetNot (TestTargetFile "FooSpec.hs")
            , someAttrs{testPath = "FooSpec.hs"}
            )
          , ( "does not match an AND target when target does not match one"
            , TestTargetAnd (TestTargetFile "FooSpec.hs") (TestTargetMarker "fast")
            , someAttrs{testPath = "BarSpec.hs", testMarkers = ["fast"]}
            )
          , ( "does not match an OR target when target does not match either"
            , TestTargetOr (TestTargetFile "FooSpec.hs") (TestTargetMarker "fast")
            , someAttrs{testPath = "BarSpec.hs", testMarkers = []}
            )
          ]
      ]

  describe "parseTestTargets" $ do
    sequence_
      [ it label $
          parseTestTargets input `shouldBe` Right (Just expected)
      | (label, input, expected) <-
          [ ( "parses file name"
            , ["test/MyLib/FooSpec.hs"]
            , TestTargetFile "test/MyLib/FooSpec.hs"
            )
          , ( "parses test name"
            , ["[test]"]
            , TestTargetName "test"
            )
          , ( "parses test marker"
            , ["@fast"]
            , TestTargetMarker "fast"
            )
          , ( "parses file name with test name"
            , ["test/FooSpec.hs[fooFunc]"]
            , TestTargetAnd (TestTargetFile "test/FooSpec.hs") (TestTargetName "fooFunc")
            )
          , ( "parses not operations"
            , ["not [fooFunc]"]
            , TestTargetNot (TestTargetName "fooFunc")
            )
          , ( "parses and operations"
            , ["[fooFunc] and @fast"]
            , TestTargetAnd (TestTargetName "fooFunc") (TestTargetMarker "fast")
            )
          , ( "parses or operations"
            , ["[fooFunc] or test/BarSpec.hs"]
            , TestTargetOr (TestTargetName "fooFunc") (TestTargetFile "test/BarSpec.hs")
            )
          , ( "joins multiple targets with or"
            , ["[fooFunc]", "test/BarSpec.hs"]
            , TestTargetOr (TestTargetName "fooFunc") (TestTargetFile "test/BarSpec.hs")
            )
          , ( "parses multiple binary operations"
            , ["[a] or [b] and [c] or [d]"]
            , TestTargetOr
                ( TestTargetAnd
                    (TestTargetOr (TestTargetName "a") (TestTargetName "b"))
                    (TestTargetName "c")
                )
                (TestTargetName "d")
            )
          , ( "parses parenthesized expressions"
            , ["([a] or [b]) and ([c] or [d])"]
            , TestTargetAnd
                (TestTargetOr (TestTargetName "a") (TestTargetName "b"))
                (TestTargetOr (TestTargetName "c") (TestTargetName "d"))
            )
          ]
      ]

    it "fails with a helpful error message" $
      parseTestTargets ["test/Example!Spec.hs"] `shouldSatisfy` P.left P.matchesSnapshot
