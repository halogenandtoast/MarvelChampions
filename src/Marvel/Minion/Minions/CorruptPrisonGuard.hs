module Marvel.Minion.Minions.CorruptPrisonGuard (
  corruptPrisonGuard,
  CorruptPrisonGuard (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Guard (printed): engaged with you, you cannot attack "the villain", the active one.
corruptPrisonGuard :: MinionCard CorruptPrisonGuard
corruptPrisonGuard = minion CorruptPrisonGuard Cards.corruptPrisonGuard (Sch 0) (Atk 2) (HP 3)

newtype CorruptPrisonGuard = CorruptPrisonGuard (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CorruptPrisonGuard where
  runMessage msg (CorruptPrisonGuard attrs) = runQueueT $ CorruptPrisonGuard <$> runMessage msg attrs
