module Marvel.Minion.Minions.Armadillo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

armadillo :: MinionCard Armadillo
armadillo = minion Armadillo Cards.armadillo (Sch 1) (Atk 2) (HP 8)

newtype Armadillo = Armadillo (Attrs Minion)
  deriving anyclass (IsMinion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Armadillo where
  getModifiersFor _ target (Armadillo a) = pure [CanHaveAnyNumberOfToughStatusCards | isTarget a target]

instance HasAbilities Armadillo where
  getAbilities (Armadillo a) =
    [ windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId $ toEnemyId a) AnyIdentity)
        ForcedResponse NoCost (RunAbility (toTarget a) 1)
    ]

instance RunMessage Armadillo where
  runMessage msg m@(Armadillo attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> m <$ enemyBecomesTough (minionId attrs)
    _ -> Armadillo <$> runMessage msg attrs
