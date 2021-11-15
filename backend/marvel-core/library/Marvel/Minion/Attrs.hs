{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Hp
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Stats
import Marvel.Target

class IsMinion a

type MinionCard a = CardBuilder MinionId a

data MinionAttrs = MinionAttrs
  { minionId :: MinionId
  , minionCardDef :: CardDef
  , minionDamage :: Natural
  , minionHitPoints :: HP Natural
  , minionScheme :: Sch
  , minionAttack :: Atk
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''MinionAttrs

instance HasCardCode MinionAttrs where
  toCardCode = toCardCode . minionCardDef

minion
  :: (MinionAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP Natural
  -> CardBuilder MinionId a
minion f cardDef sch atk hp =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \mid -> f $ MinionAttrs
      { minionId = mid
      , minionCardDef = cardDef
      , minionDamage = 0
      , minionAttack = atk
      , minionScheme = sch
      , minionHitPoints = hp
      }
    }

instance Entity MinionAttrs where
  type EntityId MinionAttrs = MinionId
  type EntityAttrs MinionAttrs = MinionAttrs
  toId = minionId
  toAttrs = id

instance IsSource MinionAttrs where
  toSource = MinionSource . toId

instance IsTarget MinionAttrs where
  toTarget = MinionTarget . toId

isTarget :: (Entity a, EntityAttrs a ~ MinionAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage MinionAttrs where
  runMessage _ = pure
