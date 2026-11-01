module Marvel.Minion.Minions.MutatedSoldier where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Stats ()

mutatedSoldier :: MinionCard MutatedSoldier
mutatedSoldier = minion MutatedSoldier Cards.mutatedSoldier (Sch 1) (Atk 2) (HP 5)

newtype MutatedSoldier = MutatedSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MutatedSoldier where
  getAbilities (MutatedSoldier attrs) =
    [windowAbility attrs 1 (W.EnemyActivates W.After (EnemyWithId $ toEnemyId attrs) AnyIdentity)
      ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage MutatedSoldier where
  runMessage msg m@(MutatedSoldier attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> m <$ heal (minionId attrs) (minionDamage attrs)
    _ -> MutatedSoldier <$> runMessage msg attrs
