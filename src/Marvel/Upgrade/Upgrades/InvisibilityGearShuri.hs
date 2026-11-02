module Marvel.Upgrade.Upgrades.InvisibilityGearShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

invisibilityGearShuri :: UpgradeCard InvisibilityGearShuri
invisibilityGearShuri = upgrade InvisibilityGearShuri Cards.invisibilityGearShuri
newtype InvisibilityGearShuri = InvisibilityGearShuri (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage InvisibilityGearShuri where
  runMessage msg u@(InvisibilityGearShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy player <- windows, player == you] $ \enemy -> replacePendingAttackWithScheme enemy you
      pure u
    _ -> InvisibilityGearShuri <$> runMessage msg attrs

instance HasAbilities InvisibilityGearShuri where
  getAbilities u@(InvisibilityGearShuri attrs) = [limitedWindowAbility u 1 (W.EnemyWouldAttack AnyEnemy You) Interrupt OwnsThis (DiscardCost $ toTarget attrs) (runAbility u 1)]
