module Marvel.Card.PlayerCard where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Card.Id

data PlayerCard = PlayerCard CardId CardDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
