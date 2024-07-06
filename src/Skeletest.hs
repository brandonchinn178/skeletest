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
  FixtureScope (..),
  FixtureCleanup (..),
  getFixture,
  noCleanup,
  withCleanup,
) where

import Skeletest.Assertions
import Skeletest.Internal.Fixtures
import Skeletest.Internal.Spec
