module Skeletest (
  -- * Spec
  Spec,
  describe,
  test,
  it,
  prop,

  -- * Assertions
  shouldBe,
  shouldNotBe,
  shouldSatisfy,
  shouldNotSatisfy,

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
