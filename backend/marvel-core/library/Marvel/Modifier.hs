module Marvel.Modifier where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Source
import Marvel.Target

newtype Modifier = ResourceCostReduction Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class HasModifiersFor a where
  getModifiersFor :: MonadGame env m => Source -> Target -> a -> m [Modifier]
