module Marvel.Minion.Minions.AdvancedUltronDrone (
  advancedUltronDrone,
  AdvancedUltronDrone (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

advancedUltronDrone :: MinionCard AdvancedUltronDrone
advancedUltronDrone =
  minion AdvancedUltronDrone Cards.advancedUltronDrone (Sch 1) (Atk 1) (HP 4)

newtype AdvancedUltronDrone = AdvancedUltronDrone (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AdvancedUltronDrone where
  runMessage msg (AdvancedUltronDrone attrs) = runQueueT $ case msg of
    -- Forced Interrupt: when defeated, the engaged player puts the top card of their
    -- deck into play as a Drone
    MinionMessage ident MinionDefeated | ident == minionId attrs -> do
      putTopOfDeckIntoPlayAsDrone (minionEngagedIdentity attrs)
      AdvancedUltronDrone <$> runMessage msg attrs
    _ -> AdvancedUltronDrone <$> runMessage msg attrs
