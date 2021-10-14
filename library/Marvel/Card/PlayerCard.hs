module Marvel.Card.PlayerCard where

import Marvel.Prelude

data PlayerCard = PlayerCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
