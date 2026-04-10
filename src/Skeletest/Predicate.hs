module Skeletest.Predicate (
  Predicate,

  -- * General
  anything,
  anythingDeep,
  anyThunk,

  -- * Ord
  eq,
  gt,
  gte,
  lt,
  lte,

  -- * Data types
  just,
  nothing,
  left,
  right,
  list,
  tup,
  con,

  -- * Numeric
  approx,
  tol,
  Tolerance (..),

  -- * Combinators
  (<<<),
  (>>>),
  not,
  (&&),
  (||),
  and,
  or,

  -- * Containers
  any,
  all,
  elem,

  -- * Subsequences
  HasSubsequences (..),
  hasPrefix,
  hasInfix,
  hasSuffix,
  empty,

  -- * IO
  returns,
  throws,

  -- * Functions
  (===),
  isoWith,

  -- * Snapshot testing
  matchesSnapshot,
) where

import Skeletest.Internal.Predicate
import Skeletest.Internal.Snapshot
import Skeletest.Prop.Internal
import Prelude ()
