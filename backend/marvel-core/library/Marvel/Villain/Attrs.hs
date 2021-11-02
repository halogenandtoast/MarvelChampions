module Marvel.Villain.Attrs (module Marvel.Villain.Attrs, module X) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity as X
import Marvel.Id as X (VillainId)

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villain :: (VillainAttrs -> a) -> CardDef -> VillainCard a
villain f cardDef = CardBuilder
  { cbCardCode = toCardCode cardDef
  , cbCardBuilder = \villainId -> f $ VillainAttrs { .. }
  }

newtype VillainAttrs = VillainAttrs { villainId :: VillainId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Entity VillainAttrs where
  type EntityId VillainAttrs = VillainId
  type EntityAttrs VillainAttrs = VillainAttrs
  toId = villainId
  toAttrs = id

