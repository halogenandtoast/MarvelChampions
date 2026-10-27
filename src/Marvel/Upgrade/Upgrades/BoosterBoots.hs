module Marvel.Upgrade.Upgrades.BoosterBoots (
  boosterBoots,
  BoosterBoots (..),
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

boosterBoots :: UpgradeCard BoosterBoots
boosterBoots = upgrade BoosterBoots Cards.boosterBoots

newtype BoosterBoots = BoosterBoots (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When you would take any amount of damage from an attack, exhaust
-- Booster Boots and discard the top card of your deck → prevent 1 of that damage." The
-- deck's top card is part of the cost: with an empty deck it cannot be paid.
instance HasAbilities BoosterBoots where
  getAbilities a@(BoosterBoots attrs) =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAttack AnyValue)
        HeroInterrupt
        OwnsThis
        (ExhaustCost <> DiscardTopOfDeckCost)
        (runAbility a 1)
    ]

instance RunMessage BoosterBoots where
  runMessage msg u@(BoosterBoots attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      preventDamage ident 1
      pure u
    _ -> BoosterBoots <$> runMessage msg attrs
