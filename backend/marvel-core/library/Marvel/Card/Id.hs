module Marvel.Card.Id where

import Marvel.Prelude

newtype CardId = CardId UUID
  deriving newtype (Show, Eq, ToJSON, FromJSON, Random)
