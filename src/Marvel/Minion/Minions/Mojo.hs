module Marvel.Minion.Minions.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

newtype MojoMinion = MojoMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39029 :: MinionCard MojoMinion
mojo39029 = minion MojoMinion Cards.mojo39029 (Sch 1) (Atk 1) (HP 6)

instance HasAbilities MojoMinion where
  getAbilities (MojoMinion a) =
    [windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage MojoMinion where
  runMessage msg e@(MojoMinion a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeCharacterThreat (minionId a) 2
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      selectList (CharacterControlledBy $ IdentityWithId you) >>= traverse_ (`placeCharacterThreat` 1)
      pure e
    _ -> MojoMinion <$> runMessage msg a
