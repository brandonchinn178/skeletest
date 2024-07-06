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

  -- * CLI flags
  Flag (..),
  IsFlag (..),
  FlagSpec (..),
  getFlag,
) where

import Skeletest.Assertions
import Skeletest.Internal.CLI
import Skeletest.Internal.Fixtures
import Skeletest.Internal.Spec
