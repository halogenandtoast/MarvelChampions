module Marvel.Card.Types (
  module Marvel.Card.Types,
  module X,
) where

import Marvel.Prelude

import Marvel.Card.Builder as X
import Marvel.Card.Code as X
import Marvel.Card.Def as X
import Marvel.Card.EncounterCard as X
import Marvel.Card.Id as X
import Marvel.Card.PlayerCard.Types as X
import Marvel.Card.Side as X

data Card = PlayerCard PlayerCard | EncounterCard EncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance HasCardCode Card where
  toCardCode = toCardCode . getCardDef

instance HasCardId Card where
  toCardId (PlayerCard pc) = toCardId pc
  toCardId (EncounterCard ec) = toCardId ec

instance HasCardDef Card where
  getCardDef (PlayerCard pc) = pcCardDef pc
  getCardDef (EncounterCard ec) = ecCardDef ec

class IsCard a where
  toCard :: a -> Card

_PlayerCard :: Traversal' Card PlayerCard
_PlayerCard f (PlayerCard pc) = PlayerCard <$> f pc
_PlayerCard _ other = pure other

_EncounterCard :: Traversal' Card EncounterCard
_EncounterCard f (EncounterCard pc) = EncounterCard <$> f pc
_EncounterCard _ other = pure other

onlyPlayerCards :: [Card] -> [PlayerCard]
onlyPlayerCards = mapMaybe (preview _PlayerCard)

onlyEncounterCards :: [Card] -> [EncounterCard]
onlyEncounterCards = mapMaybe (preview _EncounterCard)
