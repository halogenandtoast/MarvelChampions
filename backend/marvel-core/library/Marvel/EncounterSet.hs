module Marvel.EncounterSet where

import Marvel.Prelude

data EncounterSet = Rhino | BombScare
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

