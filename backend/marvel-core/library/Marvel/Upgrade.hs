{-# LANGUAGE TemplateHaskell #-}
module Marvel.Upgrade where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Upgrades
import Text.Show qualified

data Upgrade = forall a. IsUpgrade a => Upgrade a

instance Show Upgrade where
  show (Upgrade a) = show a

instance Eq Upgrade where
  (Upgrade (a :: a)) == (Upgrade (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance ToJSON Upgrade where
  toJSON (Upgrade a) = toJSON a

instance FromJSON Upgrade where
  parseJSON v = flip (withObject "Upgrade") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withUpgradeCardCode cCode $ \(_ :: UpgradeCard a) -> Upgrade <$> parseJSON @a v

withUpgradeCardCode
  :: CardCode
  -> (forall a. IsUpgrade a => UpgradeCard a -> r)
  -> r
withUpgradeCardCode cCode f =
  case lookup cCode allUpgrades of
    Nothing -> error "invalid upgrade"
    Just (SomeUpgradeCard a) -> f a

data SomeUpgradeCard = forall a. IsUpgrade a => SomeUpgradeCard (UpgradeCard a)

liftUpgradeCard :: (forall a . UpgradeCard a -> b) -> SomeUpgradeCard -> b
liftUpgradeCard f (SomeUpgradeCard a) = f a

someUpgradeCardCode :: SomeUpgradeCard -> CardCode
someUpgradeCardCode = liftUpgradeCard cbCardCode

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
lookupUpgrade cardCode identityId upgradeId = case lookup cardCode allUpgrades of
  Just (SomeUpgradeCard a) -> Upgrade $ cbCardBuilder a (identityId, upgradeId)
  Nothing -> error $ "Invalid card code for upgrade " <> show cardCode

instance Entity Upgrade where
  type EntityId Upgrade = UpgradeId
  type EntityAttrs Upgrade = UpgradeAttrs
  toId = toId . toAttrs
  toAttrs (Upgrade a) = toAttrs a

instance RunMessage Upgrade where
  runMessage msg (Upgrade a) = Upgrade <$> runMessage msg a

instance Exhaustable Upgrade where
  isExhausted = upgradeExhausted . toAttrs

instance IsSource Upgrade where
  toSource = UpgradeSource . toId

instance HasAbilities Upgrade where
  getAbilities (Upgrade a) = getAbilities a

instance HasCardDef Upgrade where
  getCardDef = getCardDef . toAttrs

instance IsCard Upgrade where
  toCard = toCard . toAttrs

getUpgradeController :: Upgrade -> IdentityId
getUpgradeController = upgradeController . toAttrs

getUpgradeUses :: Upgrade -> Natural
getUpgradeUses = upgradeUses . toAttrs

instance HasModifiersFor Upgrade where
  getModifiersFor source target (Upgrade a) = getModifiersFor source target a
