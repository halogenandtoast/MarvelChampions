module Marvel.Discard where

import Marvel.Prelude

import Marvel.Card.PlayerCard

newtype Discard = Discard { unDiscard :: [PlayerCard] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)
