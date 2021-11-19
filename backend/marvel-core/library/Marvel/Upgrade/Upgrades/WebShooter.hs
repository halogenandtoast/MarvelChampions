module Marvel.Upgrade.Upgrades.WebShooter
  ( webShooter
  , WebShooter(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

webShooter :: UpgradeCard WebShooter
webShooter = upgrade WebShooter Cards.webShooter

newtype WebShooter = WebShooter UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities WebShooter where
  getAbilities _ = []

instance RunMessage WebShooter where
  runMessage msg (WebShooter attrs) = WebShooter <$> runMessage msg attrs
