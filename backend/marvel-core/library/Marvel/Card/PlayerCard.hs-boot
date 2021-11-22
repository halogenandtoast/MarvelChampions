module Marvel.Card.PlayerCard where

import Marvel.Prelude

data PlayerCard

instance Eq PlayerCard
instance Show PlayerCard
instance FromJSON PlayerCard
instance ToJSON PlayerCard
instance Hashable PlayerCard
