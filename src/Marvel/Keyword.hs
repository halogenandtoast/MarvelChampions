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
  -- The Galaxy's Most Wanted
  | -- | defeated, it goes to the victory display, worth this many victory points (RR
    -- Victory X)
    Victory Natural
  | -- | while it is engaged with a player, they cannot thwart the main scheme
    Patrol
  | -- | it cannot be stunned or confused
    Stalwart
  | -- | while a player resolves it, the others cannot help them
    Peril
  -- Spectrum
  | -- | an energy form upgrade: an additional form, faceup while it is the one she is in
    EnergyForm
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
