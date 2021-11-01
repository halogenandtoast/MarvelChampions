module Marvel.Hand where

import Marvel.Prelude

import Marvel.Card.PlayerCard

newtype HandSize = HandSize Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Hand = Hand [PlayerCard]
  deriving newtype (Show, Eq, ToJSON, FromJSON)
