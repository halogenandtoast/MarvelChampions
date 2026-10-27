module Marvel.Upgrade.Upgrades.PlasmaPistol (
  plasmaPistol,
  PlasmaPistol (..),
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

plasmaPistol :: UpgradeCard PlasmaPistol
plasmaPistol =
  upgradeWith PlasmaPistol Cards.plasmaPistol $
    (usesL .~ 3)
      . (discardIfNoUsesL .~ True)

newtype PlasmaPistol = PlasmaPistol (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Plasma Pistol and remove 1 charge counter from it → deal 1
-- damage to an enemy.
instance HasAbilities PlasmaPistol where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> EnemyExists DamageableEnemy) (ExhaustCost <> UseCost) $ runAbility a 1]

instance RunMessage PlasmaPistol where
  runMessage msg u@(PlasmaPistol attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      chooseDamage ident attrs (toDamage 1 FromAbility) DamageableEnemy
      pure u
    _ -> PlasmaPistol <$> runMessage msg attrs
