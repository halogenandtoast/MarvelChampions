module Marvel.Minion.Minions.UltronNemesis where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W
import Marvel.Minion.Runner
import Marvel.Environment.Cards qualified as Environments
ultronNemesis :: MinionCard UltronNemesis
ultronNemesis = minion UltronNemesis Cards.ultronNemesis (Sch 2) (Atk 2) (HP 6)

newtype UltronNemesis = UltronNemesis (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities UltronNemesis where
  getAbilities (UltronNemesis a) =
    [ limitedWindowAbility a 1
        (W.EnemyAttacked W.When (EnemyWithId $ EnemyMinionId $ minionId a) $ IdentityWithId $ minionEngagedIdentity a)
        ForcedInterrupt (EnvironmentExists $ EnvironmentWithTitle "Ultron Drones") NoCost (runAbility a 1)
    ]

instance RunMessage UltronNemesis where
  runMessage msg m@(UltronNemesis attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> m <$ putTopOfDeckIntoPlayAsDrone (minionEngagedIdentity attrs)
    _ -> UltronNemesis <$> runMessage msg attrs
