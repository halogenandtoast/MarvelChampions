module Marvel.Ally.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id

class IsAlly a

type AllyCard a = CardBuilder (IdentityId, AllyId) a

data AllyAttrs = AllyAttrs
  { allyId :: AllyId
  , allyCardDef :: CardDef
  , allyDamage :: Int
  , allyThwart :: Thw
  , allyAttack :: Atk
  , allyController :: IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode AllyAttrs where
  toCardCode = toCardCode . allyCardDef

ally
  :: (AllyAttrs -> a)
  -> CardDef
  -> Thw
  -> Atk
  -> CardBuilder (IdentityId, AllyId) a
ally f cardDef thw atk = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, aid) -> f $ AllyAttrs
    { allyId = aid
    , allyCardDef = cardDef
    , allyDamage = 0
    , allyAttack = atk
    , allyThwart = thw
    , allyController = ident
    }
  }

class IsHero a

newtype Atk = Atk Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype Thw = Thw Int
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance Entity AllyAttrs where
  type EntityId AllyAttrs = AllyId
  type EntityAttrs AllyAttrs = AllyAttrs
  toId = allyId
  toAttrs = id
