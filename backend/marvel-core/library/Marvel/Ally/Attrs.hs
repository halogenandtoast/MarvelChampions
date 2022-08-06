module Marvel.Ally.Attrs (module Marvel.Ally.Attrs, module X) where

import Marvel.Prelude

import Marvel.Card as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Message as X
import Marvel.Modifier as X
import Marvel.Question as X
import Marvel.Queue as X
import Marvel.Source as X
import Marvel.Stats as X
import Marvel.Target as X

import Data.HashSet qualified as HashSet
import Marvel.Ability.Type
import Marvel.Attack
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Query
import Marvel.Window qualified as W

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ AllyAttrs, EntityId a ~ AllyId, HasModifiersFor a, HasAbilities a, RunMessage a, IsSource a) => IsAlly a

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

upgradesL :: Lens' AllyAttrs (HashSet UpgradeId)
upgradesL = lens allyUpgrades $ \m x -> m { allyUpgrades = x }

countersL :: Lens' AllyAttrs Natural
countersL = lens allyCounters $ \m x -> m { allyCounters = x }

damageL :: Lens' AllyAttrs Natural
damageL = lens allyDamage $ \m x -> m { allyDamage = x }

toughL :: Lens' AllyAttrs Bool
toughL = lens allyTough $ \m x -> m { allyTough = x }

confusedL :: Lens' AllyAttrs Bool
confusedL = lens allyConfused $ \m x -> m { allyConfused = x }

stunnedL :: Lens' AllyAttrs Bool
stunnedL = lens allyStunned $ \m x -> m { allyStunned = x }

exhaustedL :: Lens' AllyAttrs Bool
exhaustedL = lens allyExhausted $ \m x -> m { allyExhausted = x }

instance HasController AllyAttrs where
  controller = allyController

controllerMessage :: (EntityAttrs a ~ AllyAttrs, Entity a) => a -> IdentityMessage -> Message
controllerMessage a = IdentityMessage (allyController $ toAttrs a)

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

damageChoice :: AllyAttrs -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [DamageEnemy (VillainTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyAttack (toId attrs) (EnemyVillainId vid)
          ]
      ]
    ]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (toSource attrs) dmg
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyAttack (toId attrs) (EnemyMinionId vid)
          ]
      ]
    ]

thwartChoice :: AllyAttrs -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (toId attrs) (SchemeMainSchemeId vid)
          ]
      ]
    ]
  SchemeSideSchemeId sid -> TargetLabel
    (SideSchemeTarget sid)
    [ ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw
    , Run
      [ CheckWindows
          [ W.Window W.After
              $ W.AllyThwart (toId attrs) (SchemeSideSchemeId sid)
          ]
      ]
    ]

stunChoice :: (Entity a, EntityId a ~ AllyId) => a -> EnemyId -> Choice
stunChoice a = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [Stun (VillainTarget vid) (AllySource $ toId a)]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [Stun (MinionTarget vid) (AllySource $ toId a)]

instance IsCard AllyAttrs where
  toCard a = PlayerCard $ MkPlayerCard
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
                (ChooseOne $ map (damageChoice a (toDamage dmg $ FromAllyAttack (toId a))) enemies)
            : [ AllyMessage
                  ident
                  (AllyDamaged (toSource a) (toDamage (allyAttackConsequentialDamage a) FromConsequential))
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
                  (AllyDamaged (toSource a) (toDamage (allyThwartConsequentialDamage a) FromConsequential))
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
            (unHp (allyHitPoints a) - allyDamage a)
            (attackDamage attack')
        when
          (attackOverkill attack' && overkill > 0)
          (push $ IdentityMessage (allyController a) $ IdentityDamaged
            (attackSource attack')
            (toDamage overkill FromAttack)
          )

        push $ AllyMessage ident $ AllyDamaged
          (attackSource attack')
          (toDamage (attackDamage attack') FromAttack)
        pure a
      AllyDamaged _ damage -> if allyTough a
        then pure $ a & toughL .~ False
        else do
          when
            (damageAmount damage + allyDamage a >= unHp (allyHitPoints a))
            (push $ AllyMessage (toId a) AllyDefeated)
          pure $ a & damageL +~ damageAmount damage
      AllyDefeated -> do
        pushAll
          [ RemoveFromPlay (toTarget a)
          , DiscardedCard (toCard a)
          ]
        pure a
      SpendAllyUse -> pure $ a & countersL -~ 1
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    _ -> pure a
