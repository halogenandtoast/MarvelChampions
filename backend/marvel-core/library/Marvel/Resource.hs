module Marvel.Resource
  ( module Marvel.Resource
  , module Marvel.Resource.Types
  ) where

import Marvel.Prelude

import Marvel.Card.PlayerCard.Types
import Marvel.Game.Source
import Marvel.Resource.Types

class HasResources a where
  resourcesFor :: MonadGame env m => a -> Maybe PlayerCard -> m [Resource]

