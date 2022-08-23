{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Upgrade where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Upgrade.Types
import Marvel.Upgrade.Upgrades

instance FromJSON Upgrade where
  parseJSON = withObject "Upgrade" $ \o -> do
    cardDef <- o .: "upgradeCardDef"
    withUpgradeCardCode (cdCardCode cardDef)
      $ \(_ :: UpgradeCard a) -> Upgrade <$> parseJSON @a (Object o)

withUpgradeCardCode
  :: CardCode -> (forall a . IsUpgrade a => UpgradeCard a -> r) -> r
withUpgradeCardCode cCode f = case lookup cCode allUpgrades of
  Nothing -> error "invalid upgrade"
  Just (SomeUpgradeCard a) -> f a

allUpgrades :: HashMap CardCode SomeUpgradeCard
allUpgrades = fromList $ map
  (toFst someUpgradeCardCode)
  [ SomeUpgradeCard spiderTracer
  , SomeUpgradeCard webShooter
  , SomeUpgradeCard webbedUp
  , SomeUpgradeCard captainMarvelsHelmet
  , SomeUpgradeCard cosmicFlight
  -- , SomeUpgradeCard energyChannel
  , SomeUpgradeCard focusedRage
  , SomeUpgradeCard superhumanStrength
  , SomeUpgradeCard arcReactor
  , SomeUpgradeCard markVArmor
  , SomeUpgradeCard markVHelmet
  , SomeUpgradeCard poweredGauntlets
  , SomeUpgradeCard rocketBoots
  , SomeUpgradeCard energyDaggers
  , SomeUpgradeCard pantherClaws
  , SomeUpgradeCard tacticalGenius
  , SomeUpgradeCard vibraniumSuit
  , SomeUpgradeCard combatTraining
  , SomeUpgradeCard heroicIntuition
  , SomeUpgradeCard inspired
  , SomeUpgradeCard armoredVest
  , SomeUpgradeCard indomitable
  , SomeUpgradeCard tenacity
  ]

lookupUpgrade :: CardCode -> IdentityId -> UpgradeId -> Upgrade
lookupUpgrade cardCode identityId upgradeId =
  case lookup cardCode allUpgrades of
    Just (SomeUpgradeCard a) ->
      Upgrade $ cbCardBuilder a (identityId, upgradeId)
    Nothing -> error $ "Invalid card code for upgrade " <> show cardCode
