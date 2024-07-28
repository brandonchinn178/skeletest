module Skeletest (
  -- * Spec
  Spec,
  describe,
  test,
  it,
  prop,

  -- ** Modifiers
  xfail,
  skip,

  -- ** Markers
  IsMarker (..),
  withMarkers,
  withManualMarkers,
  withMarker,

  -- * Assertions
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
  context,
  failTest,
  HasCallStack,
  Predicate,
  Testable,

  -- * Properties
  Property,
  PropertyM,
  Gen,
  forAll,
  discard,

  -- ** Settings
  setDiscardLimit,
  setShrinkLimit,
  setShrinkRetries,
  setConfidence,
  setVerifiedTermination,
  setTestLimit,
  setSkipTo,

  -- * Fixtures
  Fixture (..),
  FixtureScope (..),
  FixtureCleanup (..),
  getFixture,
  noCleanup,
  withCleanup,

  -- ** Built-in fixtures
  FixtureTmpDir (..),

  -- * CLI flags
  Flag (..),
  IsFlag (..),
  FlagSpec (..),
  getFlag,
) where

import GHC.Stack (HasCallStack)

import Skeletest.Assertions
import Skeletest.Internal.CLI
import Skeletest.Internal.Fixtures
import Skeletest.Internal.Spec
import Skeletest.Predicate
import Skeletest.Prop.Gen (Gen)
import Skeletest.Prop.Internal
