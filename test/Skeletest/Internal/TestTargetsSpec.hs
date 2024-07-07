{-# LANGUAGE OverloadedStrings #-}

module Skeletest.Internal.TestTargetsSpec (spec) where

import Skeletest
import Skeletest.Predicate qualified as P

import Skeletest.Internal.TestTargets

spec :: Spec
spec = do
  describe "matchesTest" $ do
    -- TODO
    pure ()

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
