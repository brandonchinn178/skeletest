module Skeletest.Prop (
  -- * Settings
  setDiscardLimit,
  setShrinkLimit,
  setShrinkRetries,
  setConfidence,
  setVerifiedTermination,
  setTestLimit,

  -- * Coverage
  classify,
  cover,
  label,
  collect,
) where

import Skeletest.Prop.Internal
