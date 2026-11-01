module Marvel.Keyword where

import Marvel.Prelude
import Marvel.Card.Code (CardCode)

import Marvel.GameValue.Types (GameValue)
import Marvel.Resource.Types (Resource)
import Marvel.Trait.Types (Trait)

data Keyword
  = Teamwork Trait
  | Temporary
  | Alliance
  | Assault
  | Guard
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
    Victory Int
  | -- | These resources must be spent on the card, even when its cost is reduced.
    Requirement [Resource]
  | -- | while it is engaged with a player, they cannot thwart the main scheme
    Patrol
  | -- | it cannot be stunned or confused
    Stalwart
  | Steady
  | -- | while a player resolves it, the others cannot help them
    Peril
  -- Spectrum
  | -- | an energy form upgrade: an additional form, faceup while it is the one she is in
    EnergyForm
  | -- | Vision's additional form, retained even if the upgrade is blank
    MassForm
  | Linked CardCode
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
