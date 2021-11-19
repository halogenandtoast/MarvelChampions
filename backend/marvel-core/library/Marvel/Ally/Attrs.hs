{-# LANGUAGE TemplateHaskell #-}
module Marvel.Ally.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Entity
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Message
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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''AllyAttrs

instance HasCardCode AllyAttrs where
  toCardCode = toCardCode . allyCardDef

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

damageChoice :: AllyAttrs -> EnemyId -> Choice
damageChoice attrs = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [ DamageEnemy
        (VillainTarget vid)
        (AllySource $ allyId attrs)
        (unAtk $ allyAttack attrs)
    ]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [ DamageEnemy
        (MinionTarget vid)
        (AllySource $ allyId attrs)
        (unAtk $ allyAttack attrs)
    ]

thwartChoice :: AllyAttrs -> SchemeId -> Choice
thwartChoice attrs = \case
  SchemeMainSchemeId vid -> TargetLabel
    (MainSchemeTarget vid)
    [ ThwartScheme
        (MainSchemeTarget vid)
        (AllySource $ allyId attrs)
        (unThw $ allyThwart attrs)
    ]

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
      AllyAttacked -> do
        enemies <- selectList AnyEnemy
        pushAll
          $ Ask (allyController a) (ChooseOne $ map (damageChoice a) enemies)
          : [ AllyMessage
                ident
                (AllyDamaged (toSource a) (allyAttackConsequentialDamage a))
            | allyAttackConsequentialDamage a > 0
            ]
        pure a
      AllyThwarted -> do
        schemes <- selectList AnyScheme
        pushAll
          $ Ask (allyController a) (ChooseOne $ map (thwartChoice a) schemes)
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
      AllyDamaged _ damage -> do
        when
          (damage + (allyDamage a) >= unHp (allyHitPoints a))
          (push $ AllyMessage (toId a) AllyDefeated)
        pure $ a & damageL +~ damage
      AllyDefeated -> do
        pushAll
          [ RemoveFromPlay (toTarget a)
          , IdentityMessage (allyController a) (DiscardCard $ toCard a)
          ]
        pure a
    _ -> pure a
