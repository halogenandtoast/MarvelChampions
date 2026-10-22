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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
