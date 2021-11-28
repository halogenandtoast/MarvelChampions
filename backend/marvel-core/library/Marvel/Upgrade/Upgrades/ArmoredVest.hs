module Marvel.Upgrade.Upgrades.ArmoredVest
  ( armoredVest
  , ArmoredVest(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

armoredVest :: UpgradeCard ArmoredVest
armoredVest = upgrade ArmoredVest Cards.armoredVest

newtype ArmoredVest = ArmoredVest UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor ArmoredVest where
  getModifiersFor _ (IdentityTarget iid) (ArmoredVest a)
    | iid == upgradeController a = pure [DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ArmoredVest where
  getAbilities _ = []

instance RunMessage ArmoredVest where
  runMessage msg (ArmoredVest attrs) =
    ArmoredVest <$> runMessage msg attrs
