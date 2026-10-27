module Marvel.Minion.Minions.NebulaMinion (
  nebulaMinion,
  NebulaMinion (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

-- | Retaliate 2 is her keyword.
nebulaMinion :: MinionCard NebulaMinion
nebulaMinion = minion NebulaMinion Cards.nebulaMinion (Sch 1) (Atk 2) (HP 5)

newtype NebulaMinion = NebulaMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Interrupt: when this minion would enter play, discard the Nebula ally from
-- play: revealed or put into play.
instance RunMessage NebulaMinion where
  runMessage msg (NebulaMinion attrs) = runQueueT $ case msg of
    CheckWindows ws | any entering ws -> do
      traverse_ removeFromPlay =<< selectList (AllyIs Allies.nebulaAlly)
      NebulaMinion <$> runMessage msg attrs
    _ -> NebulaMinion <$> runMessage msg attrs
   where
    entering = \case
      W.Window W.When (W.MinionEnteredPlay mid) -> mid == minionId attrs
      _ -> False
