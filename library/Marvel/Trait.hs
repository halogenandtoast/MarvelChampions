module Marvel.Trait where

import Marvel.Prelude

data Trait = Genius | Shield | Soldier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
