module Skeletest (
  -- * Spec
  Spec,
  describe,
  it,
  prop,

  -- ** Modifiers
  xfail,
  skip,
  focus,
  markManual,

  -- ** Markers
  IsMarker (..),
  withMarkers,
  withMarker,

  -- * Assertions
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,
  shouldReturn,
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

  -- * Fixtures
  Fixture (..),
  FixtureScope (..),
  FixtureCleanup (..),
  getFixture,
  noCleanup,
  withCleanup,

  -- ** Built-in fixtures
  FixtureTmpDir (..),
  FixtureCapturedOutput (..),

  -- * CLI flags
  Flag (..),
  IsFlag (..),
  FlagSpec (..),
  FlagType (..),
  getFlag,
) where

import GHC.Stack (HasCallStack)
import Skeletest.Assertions
import Skeletest.Internal.CLI
import Skeletest.Internal.Capture
import Skeletest.Internal.Fixtures
import Skeletest.Internal.Spec
import Skeletest.Predicate
import Skeletest.Prop.Gen (Gen)
import Skeletest.Prop.Internal
