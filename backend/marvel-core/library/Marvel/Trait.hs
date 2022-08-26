module Marvel.Trait (module Marvel.Trait, module Marvel.Trait.Types) where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Game
import Marvel.Trait.Types

class HasTraits a where
  getTraits :: MonadGame env m => a -> m (HashSet Trait)
