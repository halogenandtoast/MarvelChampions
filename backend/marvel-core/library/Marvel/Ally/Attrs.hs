{-# LANGUAGE TemplateHaskell #-}
module Marvel.Ally.Attrs where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Attack
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Stats
import Marvel.Target

class IsAlly a

type AllyCard a = CardBuilder (IdentityId, AllyId) a

data AllyAttrs = AllyAttrs
  { allyId :: AllyId
  , allyCardDef :: CardDef
  , allyDamage :: Natural
  , allyHitPoints :: HP Natural
  , allyThwart :: Thw
  , allyThwartConsequentialDamage :: Natural
  , allyAttack :: Atk
  , allyAttackConsequentialDamage :: Natural
  , allyController :: IdentityId
  , allyExhausted :: Bool
  , allyCounters :: Natural
  , allyUpgrades :: HashSet UpgradeId
  , allyStunned :: Bool
  , allyConfused :: Bool
  , allyTough :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''AllyAttrs

instance HasCardCode AllyAttrs where
  toCardCode = toCardCode . allyCardDef

instance HasCardDef AllyAttrs where
  getCardDef = allyCardDef

allyWith
  :: (AllyAttrs -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> (AllyAttrs -> AllyAttrs)
  -> CardBuilder (IdentityId, AllyId) a
allyWith f cardDef thwPair atkPair hp g =
  ally (f . g) cardDef thwPair atkPair hp

ally
  :: (AllyAttrs -> a)
  -> CardDef
  -> (Thw, Natural)
  -> (Atk, Natural)
  -> HP Natural
  -> CardBuilder (IdentityId, AllyId) a
ally f cardDef (thw, thwConsequentialDamage) (atk, atkConsequentialDamage) hp =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, aid) -> f $ AllyAttrs
      { allyId = aid
      , allyCardDef = cardDef
      , allyDamage = 0
      , allyAttack = atk
      , allyAttackConsequentialDamage = atkConsequentialDamage
      , allyThwart = thw
      , allyThwartConsequentialDamage = thwConsequentialDamage
      , allyController = ident
      , allyHitPoints = hp
      , allyExhausted = False
      , allyStunned = False
      , allyConfused = False
      , allyTough = False
      , allyCounters = 0
      , allyUpgrades = mempty
      }
    }

instance Entity AllyAttrs where
  type EntityId AllyAttrs = AllyId
  type EntityAttrs AllyAttrs = AllyAttrs
  toId = allyId
  toAttrs = id

instance IsSource AllyAttrs where
  toSource = AllySource . toId

instance IsTarget AllyAttrs where
  toTarget = AllyTarget . toId

isTarget :: (Entity a, EntityAttrs a ~ AllyAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

getModifiedAttack :: MonadGame env m => AllyAttrs -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ allyAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedThwart :: MonadGame env m => AllyAttrs -> m Natural
getModifiedThwart attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unThw $ allyThwart attrs) modifiers
 where
  applyModifier (ThwartModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

damageChoice :: AllyAttrs -> Natural -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [DamageEnemy (VillainTarget vid) (toSource attrs) dmg]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (toSource attrs) dmg]

thwartChoice :: AllyAttrs -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw]

stunChoice :: AllyAttrs -> EnemyId -> Choice
stunChoice attrs = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [Stun (VillainTarget vid) (AllySource $ allyId attrs)]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [Stun (MinionTarget vid) (AllySource $ allyId attrs)]

toCard :: AllyAttrs -> PlayerCard
toCard a = PlayerCard
  { pcCardId = CardId $ unAllyId (allyId a)
  , pcCardDef = allyCardDef a
  , pcOwner = Just (allyController a)
  , pcController = Just (allyController a)
  }

instance RunMessage AllyAttrs where
  runMessage msg a = case msg of
    AllyMessage ident msg' | ident == allyId a -> case msg' of
      ReadiedAlly -> do
        pure $ a & exhaustedL .~ False
      ExhaustedAlly -> do
        pure $ a & exhaustedL .~ True
      AllyStunned -> do
        pure $ a & stunnedL .~ True
      AllyAttacked -> if allyStunned a
        then pure $ a & stunnedL .~ False
        else do
          enemies <- selectList AttackableEnemy
          dmg <- getModifiedAttack a
          pushAll
            $ Ask
                (allyController a)
                (ChooseOne $ map (damageChoice a dmg) enemies)
            : [ AllyMessage
                  ident
                  (AllyDamaged (toSource a) (allyAttackConsequentialDamage a))
              | allyAttackConsequentialDamage a > 0
              ]
          pure a
      AllyThwarted -> if allyConfused a
        then pure $ a & confusedL .~ False
        else do
          schemes <- selectList ThwartableScheme
          thw <- getModifiedThwart a
          pushAll
            $ Ask
                (allyController a)
                (ChooseOne $ map (thwartChoice a thw) schemes)
            : [ AllyMessage
                  ident
                  (AllyDamaged (toSource a) (allyThwartConsequentialDamage a))
              | allyThwartConsequentialDamage a > 0
              ]
          pure a
      AllyDefended enemyId -> do
        pushAll
          [ AllyMessage (toId a) ExhaustedAlly
          , case enemyId of
            EnemyVillainId vid ->
              VillainMessage vid $ VillainDefendedBy (AllyCharacter $ toId a)
            EnemyMinionId vid ->
              MinionMessage vid $ MinionDefendedBy (AllyCharacter $ toId a)
          ]
        pure a
      AllyHealed n -> pure $ a & damageL %~ subtractNatural n
      AllyWasAttacked attack' -> do
        let
          overkill = subtractNatural
            (attackDamage attack')
            (unHp (allyHitPoints a) - allyDamage a)
        when
          (attackOverkill attack' && overkill > 0)
          (push $ IdentityMessage (allyController a) $ IdentityDamaged
            (attackSource attack')
            overkill
          )

        push $ AllyMessage ident $ AllyDamaged
          (attackSource attack')
          (attackDamage attack')
        -- pushAll
        --   [ CheckWindows
        --     [ W.Window W.When
        --       $ W.AllyTakeDamage ident W.FromAttack
        --       $ attackDamage attack
        --     ]
        --   , AllyMessage ident
        --     $ AllyDamaged (attackSource attack) (attackDamage attack)
        --   ]
        pure a
      AllyDamaged _ damage -> if allyTough a
        then pure $ a & toughL .~ False
        else do
          when
            (damage + allyDamage a >= unHp (allyHitPoints a))
            (push $ AllyMessage (toId a) AllyDefeated)
          pure $ a & damageL +~ damage
      AllyDefeated -> do
        pushAll
          [ RemoveFromPlay (toTarget a)
          , IdentityMessage (allyController a) (DiscardCard $ toCard a)
          ]
        pure a
      SpendAllyUse -> pure $ a & countersL -~ 1
      UpgradeAttachedToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    _ -> pure a
