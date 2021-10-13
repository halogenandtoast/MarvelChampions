module Marvel.Villain.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villain :: (VillainAttrs -> a) -> CardDef -> VillainCard a
villain f cardDef = CardBuilder
  { cbCardCode = toCardCode cardDef
  , cbCardBuilder = \villainId -> f $ VillainAttrs { .. }
  }

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON)

newtype VillainAttrs = VillainAttrs { villainId :: VillainId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
