module Marvel.Upgrade.Upgrades.CartoonPhysics where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.GameValue
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

cartoonPhysics :: UpgradeCard CartoonPhysics
cartoonPhysics = upgrade CartoonPhysics Cards.cartoonPhysics

newtype CartoonPhysics = CartoonPhysics (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CartoonPhysics where
  getAbilities a@(CartoonPhysics attrs) =
    [limitedWindowAbility a 1
      (W.WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource (AtLeast $ Static 2))
      Interrupt (OwnsThis <> PendingDamageAtLeast 2) (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage CartoonPhysics where
  runMessage msg u@(CartoonPhysics attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      pending <- pendingDamage you
      traverse_ (preventDamage you . subtractNatural 1 . damageAmount) pending
      pure u
    _ -> CartoonPhysics <$> runMessage msg attrs
