module Marvel.Hand where

import Marvel.Prelude

import Marvel.Card.PlayerCard

newtype HandSize = HandSize { unHandSize :: Natural }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Hand = Hand { unHand :: [PlayerCard] }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

class HasHandSize a where
  handSize :: a -> HandSize
