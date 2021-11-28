module Marvel.Upgrade.Upgrades.VibraniumSuit
  ( vibraniumSuit
  , VibraniumSuit(..)
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

vibraniumSuit :: UpgradeCard VibraniumSuit
vibraniumSuit = upgrade VibraniumSuit Cards.vibraniumSuit

newtype VibraniumSuit = VibraniumSuit UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities VibraniumSuit where
  getAbilities _ = []

instance RunMessage VibraniumSuit where
  runMessage msg (VibraniumSuit attrs) = VibraniumSuit <$> runMessage msg attrs
