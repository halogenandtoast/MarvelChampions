module Marvel.Minion.Minions.MisterKnife (
  misterKnife,
  MisterKnife (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Window qualified as W

misterKnife :: MinionCard MisterKnife
misterKnife = minion MisterKnife Cards.misterKnife (Sch 2) (Atk 2) (HP 6)

newtype MisterKnife = MisterKnife (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The first treachery the engaged player reveals each villain phase gains surge: as it
-- is revealed, with Knife in play then, and only if no other treachery that player
-- revealed this phase came before it (one before Knife was in play included).
instance RunMessage MisterKnife where
  runMessage msg (MisterKnife attrs) = runQueueT $ case msg of
    CheckWindows ws | tid : _ <- [t | W.Window W.When (W.RevealTreachery t _) <- ws] -> do
      phase <- getPhase
      revealed <- getTreacheriesRevealedThisPhase
      let engaged = minionEngagedIdentity attrs
      when (phase == VillainPhase && headMay [t | (i, t) <- revealed, i == engaged] == Just tid) $
        gainSurge tid
      MisterKnife <$> runMessage msg attrs
    _ -> MisterKnife <$> runMessage msg attrs
