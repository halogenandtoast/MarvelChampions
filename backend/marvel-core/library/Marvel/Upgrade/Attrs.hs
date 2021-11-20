{-# LANGUAGE TemplateHaskell #-}
module Marvel.Upgrade.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

class IsUpgrade a

type UpgradeCard a = CardBuilder (IdentityId, UpgradeId) a

data UpgradeAttrs = UpgradeAttrs
  { upgradeId :: UpgradeId
  , upgradeCardDef :: CardDef
  , upgradeController :: IdentityId
  , upgradeExhausted :: Bool
  , upgradeAttachedEnemy :: Maybe EnemyId
  , upgradeUses :: Natural
  , upgradeDiscardIfNoUses :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''UpgradeAttrs

instance HasCardCode UpgradeAttrs where
  toCardCode = toCardCode . upgradeCardDef

upgradeWith
  :: (UpgradeAttrs -> a)
  -> CardDef
  -> (UpgradeAttrs -> UpgradeAttrs)
  -> CardBuilder (IdentityId, UpgradeId) a
upgradeWith f cardDef g = upgrade (f . g) cardDef

upgrade
  :: (UpgradeAttrs -> a) -> CardDef -> CardBuilder (IdentityId, UpgradeId) a
upgrade f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, mid) -> f $ UpgradeAttrs
    { upgradeId = mid
    , upgradeCardDef = cardDef
    , upgradeController = ident
    , upgradeExhausted = False
    , upgradeAttachedEnemy = Nothing
    , upgradeUses = 0
    , upgradeDiscardIfNoUses = False
    }
  }

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


isTarget :: (Entity a, EntityAttrs a ~ UpgradeAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage UpgradeAttrs where
  runMessage msg a = case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      PlayedUpgrade ->
        a <$ push
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
      AttachedToMinion minionId -> do
        push (MinionMessage minionId $ UpgradeAttachedToMinion (toId a))
        pure $ a & attachedEnemyL ?~ EnemyMinionId minionId
    _ -> pure a
