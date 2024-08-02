module Skeletest.Predicate (
  Predicate,

  -- * General
  anything,

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
import Prelude ()
