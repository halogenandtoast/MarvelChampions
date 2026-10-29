module Marvel.Minion.Minions.GamoraNemesis (gamoraNemesis, GamoraNemesis (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

gamoraNemesis :: MinionCard GamoraNemesis
gamoraNemesis = minion GamoraNemesis Cards.gamoraNemesis (Sch 2) (Atk 2) (HP 6)

newtype GamoraNemesis = GamoraNemesis (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GamoraNemesis where
  getAbilities (GamoraNemesis attrs) =
    [windowAbility attrs 1 (W.EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId attrs) (IdentityCharacters AnyIdentity)) ForcedResponse NoCost $ runAbility attrs 1]

instance RunMessage GamoraNemesis where
  runMessage msg m@(GamoraNemesis attrs) = runQueueT $ case msg of
    CheckWindows ws | any entering ws -> do
      traverse_ removeFromPlay =<< selectList (AllyIs Allies.gamoraNebula)
      GamoraNemesis <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 [W.EnemyAttacksAndDamages _ (IdentityCharacter you) _] _ -> do
      upgrades <- selectList $ upgradeControlledBy you <> UpgradeThatCanBeDiscarded
      unless (null upgrades) $ chooseOne you [targeting uid (discardUpgrade uid) | uid <- upgrades]
      pure m
    _ -> GamoraNemesis <$> runMessage msg attrs
   where
    entering = \case
      W.Window W.When (W.MinionEnteredPlay mid) -> mid == minionId attrs
      _ -> False
