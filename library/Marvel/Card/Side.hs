module Marvel.Card.Side where

import Marvel.Prelude

data Side = A | B | C
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
