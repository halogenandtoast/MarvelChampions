module Marvel.EncounterSet where

import Marvel.Prelude

data EncounterSet = Rhino | BombScare | Standard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

