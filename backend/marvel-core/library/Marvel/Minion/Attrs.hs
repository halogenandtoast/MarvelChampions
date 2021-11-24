{-# LANGUAGE TemplateHaskell #-}
module Marvel.Minion.Attrs where

import Marvel.Prelude

import qualified Data.HashSet as HashSet
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import qualified Marvel.Window as W

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
  , minionTough :: Bool
  , minionAttacking :: Maybe CharacterId
  , minionUpgrades :: HashSet UpgradeId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''MinionAttrs

instance HasCardCode MinionAttrs where
  toCardCode = toCardCode . minionCardDef

minionWith
  :: (MinionAttrs -> a)
  -> CardDef
  -> Sch
  -> Atk
  -> HP Natural
  -> (MinionAttrs -> MinionAttrs)
  -> CardBuilder (IdentityId, MinionId) a
minionWith f cardDef sch atk hp g = minion (f . g) cardDef sch atk hp

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
    , minionTough = False
    , minionAttacking = Nothing
    , minionUpgrades = mempty
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

instance RunMessage MinionAttrs where
  runMessage msg attrs = case msg of
    MinionMessage minionId msg' | minionId == toId attrs ->
      runMinionMessage msg' attrs
    _ -> pure attrs

runMinionMessage
  :: MonadGame env m => MinionMessage -> MinionAttrs -> m MinionAttrs
runMinionMessage msg attrs = case msg of
  MinionHealed n -> do
    pure $ attrs & damageL %~ subtractNatural n
  MinionDamaged _ damage -> if minionTough attrs
    then pure $ attrs & toughL .~ False
    else do
      when
        (damage + minionDamage attrs >= unHp (minionHitPoints attrs))
        (pushAll
          [ CheckWindows [W.Window W.When $ W.DefeatedMinion (toId attrs)]
          , MinionMessage (toId attrs) MinionDefeated
          ]
        )
      pure $ attrs & damageL +~ damage
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
  MinionDefendedBy characterId -> pure $ attrs & attackingL ?~ characterId
  UpgradeAttachedToMinion upgradeId ->
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  RevealMinion _ -> pure attrs
  MinionDefeated -> do
    pushAll
      $ map (RemoveFromPlay . UpgradeTarget) (toList $ minionUpgrades attrs)
      <> [ RemoveFromPlay (toTarget attrs)
         , IdentityMessage
           (minionEngagedIdentity attrs)
           (MinionDisengaged $ toId attrs)
         ]
    pure attrs
  MinionSchemes -> if minionConfused attrs
    then pure $ attrs & confusedL .~ False
    else do
      push $ MinionMessage (toId attrs) MinionSchemed
      pure attrs
  MinionAttacks ident -> if minionStunned attrs
    then pure $ attrs & stunnedL .~ False
    else do
      pushAll
        [ CheckWindows
          [W.Window W.Would $ W.EnemyAttack (EnemyMinionId $ toId attrs) ident]
        , MinionMessage (toId attrs) (MinionBeginAttack ident)
        ]
      pure attrs
  MinionBeginAttack ident -> do
    pushAll
      [ CheckWindows
        [W.Window W.When $ W.EnemyAttack (EnemyMinionId $ toId attrs) ident]
      , DeclareDefense ident (EnemyMinionId (toId attrs))
      , MinionMessage (toId attrs) MinionAttacked
      ]
    pure $ attrs & attackingL ?~ IdentityCharacter ident
  MinionSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        let threat = unSch (minionScheme attrs)
        pushAll
          [ CheckWindows
            [ W.Window W.Would
                $ W.ThreatPlaced (SchemeMainSchemeId mainSchemeId) threat
            ]
          , MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat
          ]
        pure attrs
      _ -> error "Not the main scheme"
  MinionAttacked -> do
    let dmg = unAtk (minionAttack attrs)
    case minionAttacking attrs of
      Just (IdentityCharacter ident) -> pushAll
        [ CheckWindows
          [W.Window W.When $ W.IdentityTakeDamage ident W.FromAttack dmg]
        , IdentityMessage ident $ IdentityDamaged (toSource attrs) dmg
        ]
      Just (AllyCharacter ident) ->
        push (AllyMessage ident $ AllyDamaged (toSource attrs) dmg)
      _ -> error "Invalid damage target"
    pure attrs
