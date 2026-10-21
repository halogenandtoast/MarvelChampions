module Marvel.Card.Id where

import Marvel.Prelude

newtype CardId = CardId { unCardId :: UUID }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Random, Hashable)

class HasCardId a where
  toCardId :: a -> CardId

