module Marvel.Card.PlayerCard.Types where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Id
import Marvel.Ref

data PlayerCard = MkPlayerCard
  { pcCardId :: CardId
  , pcCardDef :: CardDef
  , pcOwner :: Maybe IdentityId
  , pcController :: Maybe IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance IsRef PlayerCard where
  toRef = CardIdRef . pcCardId

instance HasCardId PlayerCard where
  toCardId = pcCardId

instance HasCardCode PlayerCard where
  toCardCode = toCardCode . getCardDef

instance HasCardDef PlayerCard where
  getCardDef = pcCardDef

-- | System Shock (The Mad Titan's Shadow campaign): drawn, it stays in hand, where its
-- text works (project owner's ruling), rather than going into play as an obligation, and
-- it cannot be chosen to discard from there. ponytail: by its code, the one such card.
staysInHand :: PlayerCard -> Bool
staysInHand = (== "21185") . toCardCode
