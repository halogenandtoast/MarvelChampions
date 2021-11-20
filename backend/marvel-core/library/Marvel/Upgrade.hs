{-# LANGUAGE TemplateHaskell #-}
module Marvel.Upgrade where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.TH
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Upgrades

$(buildEntity "Upgrade")

allUpgrades :: HashMap CardCode (IdentityId -> UpgradeId -> Upgrade)
allUpgrades = fromList $ map
  (toCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Upgrade")

lookupUpgrade :: CardCode -> (IdentityId -> UpgradeId -> Upgrade)
lookupUpgrade cardCode = case lookup cardCode allUpgrades of
  Just f -> f
  Nothing -> error $ "Invalid card code for upgrade " <> show cardCode

instance Entity Upgrade where
  type EntityId Upgrade = UpgradeId
  type EntityAttrs Upgrade = UpgradeAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Upgrade where
  runMessage = genericRunMessage

instance Exhaustable Upgrade where
  isExhausted = upgradeExhausted . toAttrs

instance IsSource Upgrade where
  toSource = UpgradeSource . toId

instance HasAbilities Upgrade where
  getAbilities = genericGetAbilities

instance HasCardDef Upgrade where
  getCardDef = getCardDef . toAttrs

getUpgradeController :: Upgrade -> IdentityId
getUpgradeController = upgradeController . toAttrs

getUpgradeUses :: Upgrade -> Natural
getUpgradeUses = upgradeUses . toAttrs
