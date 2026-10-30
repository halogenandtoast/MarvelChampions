module Marvel.Upgrade.Upgrades.ForcefieldGenerator where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

forcefieldGenerator :: UpgradeCard ForcefieldGenerator
forcefieldGenerator = upgradeWith ForcefieldGenerator Cards.forcefieldGenerator ((usesL .~ 6) . (discardIfNoUsesL .~ True))

newtype ForcefieldGenerator = ForcefieldGenerator (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ForcefieldGenerator where
  getAbilities a@(ForcefieldGenerator attrs) =
    [limitedWindowAbility a 1 (W.WouldTakeDamage (IdentityWithId $ upgradeController attrs) FromAnyDamageSource AnyValue)
       ForcedInterrupt OwnsThis NoCost (runAbility a 1) | upgradeUses attrs > 0]

instance RunMessage ForcefieldGenerator where
  runMessage msg u@(ForcefieldGenerator attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let incoming = maximum (0 : [damageAmount damage | W.IdentityTakeDamage ident damage <- windows, ident == you])
          n = min incoming (upgradeUses attrs)
      removeCounters attrs n
      preventDamage you n
      pure u
    _ -> ForcefieldGenerator <$> runMessage msg attrs
