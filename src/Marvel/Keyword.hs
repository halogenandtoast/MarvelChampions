module Marvel.Keyword where

import Marvel.Prelude

import Marvel.GameValue.Types (GameValue)

data Keyword
  = Guard
  | Toughness
  | Quickstrike
  | Retaliate Natural
  | Surge
  | -- | a player controls no more than two restricted cards, and those modifiers add
    Restricted
  | Overkill
  | -- | on reveal, place this much threat on the main scheme
    Incite Natural
  | -- | enters play with this much more threat (RR Hinder X)
    Hinder GameValue
  | Piercing
  | Ranged
  | Villainous
  | Permanent
  | Setup
  | -- | on a minion: the player it is engaged with cannot thwart the main scheme
    Patrol
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
