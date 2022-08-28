module Marvel.Trait
  ( module Marvel.Trait
  , module Marvel.Trait.Types
  ) where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Trait.Types

class HasTraits a where
  getTraits :: HasGame m => a -> m (HashSet Trait)
