module Marvel.Keyword where

import Marvel.Prelude

data Keyword
  = Guard
  | Toughness
  | Quickstrike
  | Retaliate Natural
  | Surge
  | -- | a player controls no more than two restricted cards
    Restricted
  | Overkill
  | -- | on reveal, place this much threat on the main scheme
    Incite Natural
  | Piercing
  | Ranged
  | Villainous
  | Permanent
  | Setup
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
