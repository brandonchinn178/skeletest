module Skeletest.Internal.TestSelection (
  TestSelections,
  TestSelection (..),
  TestTargets,
  TestTarget (..),
  Expr (..),
  parseTestSelections,
) where

import Data.Text (Text)

data Expr a
  = ExprBase a
  | ExprNot (Expr a)
  | ExprAnd (Expr a) (Expr a)
  | ExprOr (Expr a) (Expr a)

type TestSelections = Maybe (Expr TestSelection)

data TestSelection
  = TestSelectInFile FilePath (Maybe TestTargets)
  | TestSelectAnyFile TestTargets

type TestTargets = Expr TestTarget

data TestTarget
  = TestTargetName Text
  | TestTargetMarker Text

parseTestSelections :: [Text] -> Either Text TestSelections
parseTestSelections [] = Right Nothing
parseTestSelections _ = undefined
