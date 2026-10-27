module Marvel.Upgrade.Upgrades.ArmorPlating (
  armorPlating,
  ArmorPlating (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

armorPlating :: UpgradeCard ArmorPlating
armorPlating = upgrade ArmorPlating Cards.armorPlating

newtype ArmorPlating = ArmorPlating (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When an identity would take any amount of damage, exhaust Armor
-- Plating → prevent 1 of that damage (2 of that damage instead if you control the
-- Milano). Any player's identity, from any source; the Milano as the effect resolves.
instance HasAbilities ArmorPlating where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (WouldTakeDamage AnyIdentity FromAnyDamageSource (GreaterThan $ Static 0))
        HeroInterrupt
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage ArmorPlating where
  runMessage msg u@(ArmorPlating attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 [IdentityTakeDamage ident _] _ -> do
      milano <- controlsMilano you
      preventDamage ident (if milano then 2 else 1)
      pure u
    _ -> ArmorPlating <$> runMessage msg attrs
