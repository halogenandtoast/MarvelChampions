module Marvel.Upgrade.Upgrades.EnergyBarrier (
  energyBarrier,
  EnergyBarrier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

energyBarrier :: UpgradeCard EnergyBarrier
energyBarrier =
  upgradeWith EnergyBarrier Cards.energyBarrier $
    (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype EnergyBarrier = EnergyBarrier (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnergyBarrier where
  getAbilities a@(EnergyBarrier attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue)
        Interrupt
        OwnsThis
        UseCost
        (runAbility a 1)
    ]

instance RunMessage EnergyBarrier where
  runMessage msg a@(EnergyBarrier attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      preventDamage ident 1
      chooseDamage ident a (toDamage 1 FromAbility) AnyEnemy
      pure a
    _ -> EnergyBarrier <$> runMessage msg attrs
