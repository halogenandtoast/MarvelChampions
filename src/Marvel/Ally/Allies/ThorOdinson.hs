module Marvel.Ally.Allies.ThorOdinson (thorOdinson, ThorOdinson (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Resource
import Marvel.Window qualified as W

thorOdinson :: AllyCard ThorOdinson
thorOdinson = allyWith ThorOdinson Cards.thorOdinson (Thw 1, 1) (Atk 3, 1) (HP 3) (toughL .~ True)

newtype ThorOdinson = ThorOdinson (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ThorOdinson where
  getAbilities a@(ThorOdinson attrs) =
    [ limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId attrs) MinionEnemy)
        Interrupt OwnsThis (ResourceCost $ Just Energy) (runAbility a 1)
    ]

instance RunMessage ThorOdinson where
  runMessage msg a@(ThorOdinson attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [W.AllyAttack _ (EnemyMinionId target)] _ -> do
      player <- selectOne (IdentityEngagedWith $ MinionWithId target)
      for_ player $ \engaged -> do
        minions <- selectList (MinionEngagedWith $ IdentityWithId engaged)
        resolveAllyAttackAgainstAll (allyId attrs) (map EnemyMinionId minions)
      pure a
    _ -> ThorOdinson <$> runMessage msg attrs
