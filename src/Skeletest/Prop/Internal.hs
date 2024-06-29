module Skeletest.Prop.Internal (
  Property,
  runProperty,
) where

data Property

runProperty :: Property -> IO ()
runProperty _ = pure ()
