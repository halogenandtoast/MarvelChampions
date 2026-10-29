module Marvel.Upgrade.Upgrades.SolarGem where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Resource (Resource (Wild))
import Marvel.Payment (Payment (ResourcePayment))
solarGem :: UpgradeCard SolarGem
solarGem = upgrade SolarGem Cards.solarGem

newtype SolarGem = SolarGem (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SolarGem where
  getModifiersFor _ (IdentityRef you) (SolarGem a) | you == upgradeController a = pure [TraitModifier Trait.Aerial]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SolarGem where
  getAbilities a = [ability a 1 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage SolarGem where
  runMessage msg (SolarGem attrs) = SolarGem <$> runMessage msg attrs
