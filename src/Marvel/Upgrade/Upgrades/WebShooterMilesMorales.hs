module Marvel.Upgrade.Upgrades.WebShooterMilesMorales where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Payment
import Marvel.Question
import Marvel.Resource

webShooterMilesMorales :: UpgradeCard WebShooterMilesMorales
webShooterMilesMorales = upgradeWith WebShooterMilesMorales Cards.webShooterMilesMorales $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype WebShooterMilesMorales = WebShooterMilesMorales (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WebShooterMilesMorales where
  getAbilities a = [ability a 1 HeroResource (OwnsThis <> InHeroForm) (ExhaustCost <> UseCost) (Pay $ ResourcePayment Wild)]

instance RunMessage WebShooterMilesMorales where
  runMessage msg (WebShooterMilesMorales attrs) = WebShooterMilesMorales <$> runMessage msg attrs
