{-# LANGUAGE CPP #-}

module Skeletest.Internal.GHC.Compat (module X) where

#if __GLASGOW_HASKELL__ == 908
import Skeletest.Internal.GHC.Compat_9_8 as X
#elif __GLASGOW_HASKELL__ == 910
import Skeletest.Internal.GHC.Compat_9_10 as X
#elif __GLASGOW_HASKELL__ == 912
import Skeletest.Internal.GHC.Compat_9_12 as X
#endif
