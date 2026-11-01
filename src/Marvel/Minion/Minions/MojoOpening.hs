module Marvel.Minion.Minions.MojoOpening where

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Window qualified as W

newtype MojoOpeningMinion = MojoOpeningMinion (Attrs Minion)
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39007 :: MinionCard MojoOpeningMinion
mojo39007 = minion MojoOpeningMinion Cards.mojo39007 (Sch 1) (Atk 2) (HP 7)

instance HasAbilities MojoOpeningMinion where
  getAbilities (MojoOpeningMinion a) =
    [windowAbility a 1 (W.EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) AnyCharacter) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage MojoOpeningMinion where
  runMessage msg e@(MojoOpeningMinion a) = runQueueT $ case msg of
    MinionMessage mid MinionDefeated | mid == minionId a -> do
      players <- fromIntegral <$> getStartingPlayerCount
      rateChallengers (2 * players)
      MojoOpeningMinion <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ rateChampion 1
    _ -> MojoOpeningMinion <$> runMessage msg a

instance HasModifiersFor MojoOpeningMinion where
  getModifiersFor _ target (MojoOpeningMinion a)
    | target == toRef a = perPlayerHitPoints a 7
    | otherwise = pure []
