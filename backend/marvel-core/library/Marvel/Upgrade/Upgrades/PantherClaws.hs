module Marvel.Upgrade.Upgrades.PantherClaws
  ( pantherClaws
  , PantherClaws(..)
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

pantherClaws :: UpgradeCard PantherClaws
pantherClaws = upgrade PantherClaws Cards.pantherClaws

newtype PantherClaws = PantherClaws UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities PantherClaws where
  getAbilities _ = []

instance RunMessage PantherClaws where
  runMessage msg (PantherClaws attrs) = PantherClaws <$> runMessage msg attrs
