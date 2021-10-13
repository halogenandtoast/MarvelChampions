module Marvel.Trait where

import Marvel.Prelude

data Trait
  = Brute
  | Criminal
  | Genius
  | MastersOfEvil
  | Shield
  | Soldier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
