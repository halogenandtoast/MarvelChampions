{-# LANGUAGE TemplateHaskell #-}

module Marvel.Upgrade.Attrs where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W

class IsUpgrade a

type UpgradeCard a = CardBuilder (IdentityId, UpgradeId) a

data UpgradeAttrs = UpgradeAttrs
  { upgradeId :: UpgradeId
  , upgradeCardDef :: CardDef
  , upgradeController :: IdentityId
  , upgradeExhausted :: Bool
  , upgradeAttachedEnemy :: Maybe EnemyId
  , upgradeAttachedAlly :: Maybe AllyId
  , upgradeUses :: Natural
  , upgradeDiscardIfNoUses :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''UpgradeAttrs

instance HasCardCode UpgradeAttrs where
  toCardCode = toCardCode . upgradeCardDef

upgradeWith ::
  (UpgradeAttrs -> a) ->
  CardDef ->
  (UpgradeAttrs -> UpgradeAttrs) ->
  CardBuilder (IdentityId, UpgradeId) a
upgradeWith f cardDef g = upgrade (f . g) cardDef

upgrade ::
  (UpgradeAttrs -> a) -> CardDef -> CardBuilder (IdentityId, UpgradeId) a
upgrade f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, mid) ->
        f $
          UpgradeAttrs
            { upgradeId = mid
            , upgradeCardDef = cardDef
            , upgradeController = ident
            , upgradeExhausted = False
            , upgradeAttachedEnemy = Nothing
            , upgradeAttachedAlly = Nothing
            , upgradeUses = 0
            , upgradeDiscardIfNoUses = False
            }
    }

damageChoice :: UpgradeAttrs -> W.DamageSource -> Natural -> EnemyId -> Choice
damageChoice attrs damageSource dmg = \case
  EnemyVillainId vid ->
    TargetLabel
      (VillainTarget vid)
      [DamageEnemy (VillainTarget vid) (toSource attrs) damageSource dmg]
  EnemyMinionId vid ->
    TargetLabel
      (MinionTarget vid)
      [DamageEnemy (MinionTarget vid) (toSource attrs) damageSource dmg]

thwartChoice :: UpgradeAttrs -> Natural -> SchemeId -> Choice
thwartChoice attrs thw = \case
  SchemeMainSchemeId vid ->
    TargetLabel
      (MainSchemeTarget vid)
      [ ThwartScheme (MainSchemeTarget vid) (toSource attrs) thw
      ]
  SchemeSideSchemeId sid ->
    TargetLabel
      (SideSchemeTarget sid)
      [ ThwartScheme (SideSchemeTarget sid) (toSource attrs) thw
      ]

instance Entity UpgradeAttrs where
  type EntityId UpgradeAttrs = UpgradeId
  type EntityAttrs UpgradeAttrs = UpgradeAttrs
  toId = upgradeId
  toAttrs = id

instance IsSource UpgradeAttrs where
  toSource = UpgradeSource . toId

instance IsTarget UpgradeAttrs where
  toTarget = UpgradeTarget . toId

instance HasCardDef UpgradeAttrs where
  getCardDef = upgradeCardDef

instance IsCard UpgradeAttrs where
  toCard a =
    PlayerCard $
      MkPlayerCard
        { pcCardId = CardId $ unUpgradeId $ toId a
        , pcCardDef = getCardDef a
        , pcOwner = Just (upgradeController a)
        , pcController = Just (upgradeController a)
        }

instance RunMessage UpgradeAttrs where
  runMessage msg a = case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      PlayedUpgrade ->
        a
          <$ push
            (IdentityMessage (upgradeController a) $ UpgradeCreated (toId a))
      ReadiedUpgrade -> do
        pure $ a & exhaustedL .~ False
      SpendUpgradeUse -> do
        when
          (upgradeUses a == 1 && upgradeDiscardIfNoUses a)
          (push $ RemoveFromPlay (toTarget a))
        pure $ a & usesL -~ 1
      ExhaustedUpgrade -> do
        pure $ a & exhaustedL .~ True
      UpgradeAttachedToEnemy enemyId -> do
        case enemyId of
          EnemyMinionId minionId ->
            push (MinionMessage minionId $ AttachedUpgradeToMinion (toId a))
          EnemyVillainId villainId ->
            push (VillainMessage villainId $ AttachedUpgradeToVillain (toId a))
        pure $ a & attachedEnemyL ?~ enemyId
      UpgradeAttachedToAlly allyId -> do
        push (AllyMessage allyId $ AttachedUpgradeToAlly (toId a))
        pure $ a & attachedAllyL ?~ allyId
    _ -> pure a
