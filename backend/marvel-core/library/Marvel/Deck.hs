module Marvel.Deck where

import Marvel.Prelude

import Marvel.Card.PlayerCard.Types

newtype Deck = Deck { unDeck :: [PlayerCard] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)
