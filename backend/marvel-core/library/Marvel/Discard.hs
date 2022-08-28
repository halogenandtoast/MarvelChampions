module Marvel.Discard where

import Marvel.Prelude

import Marvel.Card.PlayerCard.Types

newtype Discard = Discard { unDiscard :: [PlayerCard] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)
