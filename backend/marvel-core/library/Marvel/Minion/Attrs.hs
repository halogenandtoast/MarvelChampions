{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target

class IsMinion a

type MinionCard a = CardBuilder (IdentityId, MinionId) a

data MinionAttrs = MinionAttrs
  { minionId :: MinionId
  , minionCardDef :: CardDef
  , minionDamage :: Natural
  , minionHitPoints :: HP Natural
  , minionScheme :: Sch
  , minionAttack :: Atk
  , minionEngagedIdentity :: IdentityId
  , minionStunned :: Bool
  , minionConfused :: Bool
  , minionAttacking :: Maybe CharacterId
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
  -> CardBuilder (IdentityId, MinionId) a
minion f cardDef sch atk hp = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, mid) -> f $ MinionAttrs
    { minionId = mid
    , minionCardDef = cardDef
    , minionDamage = 0
    , minionAttack = atk
    , minionScheme = sch
    , minionHitPoints = hp
    , minionEngagedIdentity = ident
    , minionStunned = False
    , minionConfused = False
    , minionAttacking = Nothing
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

instance HasCardDef MinionAttrs where
  getCardDef = minionCardDef

isTarget :: (Entity a, EntityAttrs a ~ MinionAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage MinionAttrs where
  runMessage msg attrs = case msg of
    MinionMessage minionId msg' | minionId == toId attrs ->
      runMinionMessage msg' attrs
    _ -> pure attrs

runMinionMessage
  :: MonadGame env m => MinionMessage -> MinionAttrs -> m MinionAttrs
runMinionMessage msg attrs = case msg of
  MinionDamaged _ damage -> do
    when
      (damage + minionDamage attrs >= unHp (minionHitPoints attrs))
      (push $ MinionMessage (toId attrs) MinionDefeated)
    pure $ attrs & damageL +~ damage
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
  MinionDefendedBy characterId -> pure $ attrs & attackingL ?~ characterId
  RevealMinion -> pure attrs
  MinionDefeated -> do
    pushAll
      [ RemoveFromPlay (toTarget attrs)
      , IdentityMessage
        (minionEngagedIdentity attrs)
        (MinionDisengaged $ toId attrs)
      ]
    pure attrs
