module Marvel.Minion.Minions.JoystickShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W
import Marvel.Card

import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Minion.Runner

joystickShuri :: MinionCard JoystickShuri
joystickShuri = minion JoystickShuri Cards.joystickShuri (Sch 1) (Atk 1) (HP 17)
newtype JoystickShuri = JoystickShuri (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage JoystickShuri where
  runMessage msg u@(JoystickShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you [labeled "$label.bpRiskBoost" (dealBoost attrs >> drawCards you 1), labeled "$label.giveTough" (enemyBecomesTough (minionId attrs))]
      pure u
    _ -> JoystickShuri <$> runMessage msg attrs

instance HasAbilities JoystickShuri where
  getAbilities u@(JoystickShuri attrs) = [limitedWindowAbility u 1 (W.EnemyActivates W.When (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility u 1)]
