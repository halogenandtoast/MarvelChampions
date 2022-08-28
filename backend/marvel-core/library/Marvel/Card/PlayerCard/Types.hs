module Marvel.Card.PlayerCard.Types where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Id

data PlayerCard = MkPlayerCard
  { pcCardId :: CardId
  , pcCardDef :: CardDef
  , pcOwner :: Maybe IdentityId
  , pcController :: Maybe IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance HasCardId PlayerCard where
  toCardId = pcCardId

instance HasCardCode PlayerCard where
  toCardCode = toCardCode . getCardDef

instance HasCardDef PlayerCard where
  getCardDef = pcCardDef

