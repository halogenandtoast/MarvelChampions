module Marvel.Upgrade.Upgrades.PlanB where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

planB :: UpgradeCard PlanB
planB = upgrade PlanB Cards.planB

newtype PlanB = PlanB (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PlanB where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> EnemyExists DamageableEnemy)
      (ExhaustCost <> DiscardRandomHandCardsCost 1) (runAbility a 1)]

instance RunMessage PlanB where
  runMessage msg a@(PlanB attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ ->
      a <$ chooseDamage you attrs (toDamage 2 FromAbility) DamageableEnemy
    _ -> PlanB <$> runMessage msg attrs
