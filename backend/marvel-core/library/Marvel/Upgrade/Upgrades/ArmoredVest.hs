module Marvel.Upgrade.Upgrades.ArmoredVest (
  armoredVest,
  ArmoredVest (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

armoredVest :: UpgradeCard ArmoredVest
armoredVest = upgrade ArmoredVest Cards.armoredVest

newtype ArmoredVest = ArmoredVest (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor ArmoredVest where
  getModifiersFor _ (IdentityRef iid) (ArmoredVest a)
    | iid == upgradeController a = pure [DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ArmoredVest where
  getAbilities _ = []

instance RunMessage ArmoredVest where
  runMessage msg (ArmoredVest attrs) =
    ArmoredVest <$> runMessage msg attrs
