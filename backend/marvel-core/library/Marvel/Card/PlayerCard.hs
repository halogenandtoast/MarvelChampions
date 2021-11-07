module Marvel.Card.PlayerCard where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Id

data PlayerCard = PlayerCard
  { pcCardId :: CardId
  , pcCardDef :: CardDef
  , pcCardType :: CardType
  , pcOwner :: Maybe IdentityId
  , pcController :: Maybe IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef PlayerCard where
  getCardDef = pcCardDef

instance HasResources PlayerCard where
  resourcesFor x c = do
    guard $ x /= c
    cdResources $ getCardDef x
