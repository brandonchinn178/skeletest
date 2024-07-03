module Skeletest (
  -- * Spec
  Spec,
  describe,
  test,
  it,
  prop,

  -- * Assertions
  shouldBe,
  shouldSatisfy,

  -- * Fixtures
  Fixture (..),
  FixtureDef (..),
  FixtureScope (..),
  FixtureCleanup (..),
  getFixture,
  cleanupFixtures,
) where

import Skeletest.Assertions
import Skeletest.Internal.Fixtures
import Skeletest.Internal.Spec
