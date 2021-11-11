module Marvel.Keyword where

import Marvel.Prelude

data Keyword = Guard | Toughness
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
