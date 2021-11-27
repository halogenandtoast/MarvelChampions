module Marvel.Card.EncounterCard where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id

data EncounterCard = EncounterCard
  { ecCardId :: CardId
  , ecCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance HasCardCode EncounterCard where
  toCardCode = toCardCode . getCardDef

instance HasCardDef EncounterCard where
  getCardDef = ecCardDef
