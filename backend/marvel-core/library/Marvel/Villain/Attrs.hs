module Marvel.Villain.Attrs
  ( module Marvel.Villain.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity as X
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id as X (VillainId)
import Marvel.Stats

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villain
  :: (VillainAttrs -> a) -> CardDef -> Sch -> Atk -> GameValue -> VillainCard a
villain f cardDef sch atk startingHP = CardBuilder
  { cbCardCode = toCardCode cardDef
  , cbCardBuilder = \ident -> f $ VillainAttrs
    { villainId = ident
    , villainCardDef = cardDef
    , villainStartingHP = startingHP
    , villainMaxHP = startingHP
    , villainHP = HP $ Static 1 -- placeholder
    }
  }

data VillainAttrs = VillainAttrs
  { villainId :: VillainId
  , villainCardDef :: CardDef
  , villainHP :: HP
  , villainStartingHP :: GameValue
  , villainMaxHP :: GameValue
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Entity VillainAttrs where
  type EntityId VillainAttrs = VillainId
  type EntityAttrs VillainAttrs = VillainAttrs
  toId = villainId
  toAttrs = id

