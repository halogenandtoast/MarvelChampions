module Marvel.Minion.Minions.GoblinThrall (
  goblinThrall,
  GoblinThrall (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Game.Source

goblinThrall :: MinionCard GoblinThrall
goblinThrall = minion GoblinThrall Cards.goblinThrall (Sch 1) (Atk 1) (HP 3)

newtype GoblinThrall = GoblinThrall (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GoblinThrall where
  runMessage msg e@(GoblinThrall attrs) = runQueueT $ case msg of
    -- Boost: put this minion into play engaged with you
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      e <$ putBoostIntoPlay target you
    _ -> GoblinThrall <$> runMessage msg attrs
