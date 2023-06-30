module Marvel.Upgrade.Upgrades.MarkVArmor (
  markVArmor,
  MarkVArmor (..),
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

markVArmor :: UpgradeCard MarkVArmor
markVArmor = upgrade MarkVArmor Cards.markVArmor

newtype MarkVArmor = MarkVArmor (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor MarkVArmor where
  getModifiersFor _ (IdentityRef ident) (MarkVArmor a)
    | ident == upgradeController a = pure [HitPointModifier 6]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MarkVArmor where
  getAbilities _ = []

instance RunMessage MarkVArmor where
  runMessage msg (MarkVArmor attrs) = MarkVArmor <$> runMessage msg attrs
