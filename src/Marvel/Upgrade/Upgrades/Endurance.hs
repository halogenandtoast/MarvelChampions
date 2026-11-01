module Marvel.Upgrade.Upgrades.Endurance (
  endurance,
  Endurance (..),
) where

import Marvel.Prelude
import Marvel.Ability ( HasAbilities )


import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

endurance :: UpgradeCard Endurance
endurance = upgrade Endurance Cards.endurance

newtype Endurance = Endurance (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor Endurance where
  getModifiersFor _ (IdentityRef iid) (Endurance a)
    | iid == upgradeController a = pure [HitPointModifier 3]
  getModifiersFor _ _ _ = pure []

instance RunMessage Endurance where
  runMessage msg (Endurance attrs) = Endurance <$> runMessage msg attrs
