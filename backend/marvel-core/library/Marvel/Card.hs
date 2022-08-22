module Marvel.Card
  ( module Marvel.Card
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder as X
import Marvel.Card.Code as X
import Marvel.Card.Def as X
import Marvel.Card.EncounterCard as X
import Marvel.Card.Id as X
import Marvel.Card.PlayerCard as X
import Marvel.Card.Side as X

data Card = PlayerCard PlayerCard | EncounterCard EncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance HasCardCode Card where
  toCardCode = toCardCode . getCardDef

instance HasCardDef Card where
  getCardDef (PlayerCard pc) = pcCardDef pc
  getCardDef (EncounterCard ec) = ecCardDef ec

instance HasResources Card where
  resourcesFor (EncounterCard _) _ = pure []
  resourcesFor (PlayerCard x) mc = resourcesFor x mc

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
