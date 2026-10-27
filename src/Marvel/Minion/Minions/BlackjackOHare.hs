module Marvel.Minion.Minions.BlackjackOHare (
  blackjackOHare,
  BlackjackOHare (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | Quickstrike and villainous are its keywords.
blackjackOHare :: MinionCard BlackjackOHare
blackjackOHare = minion BlackjackOHare Cards.blackjackOHare (Sch 1) (Atk 1) (HP 3)

newtype BlackjackOHare = BlackjackOHare (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BlackjackOHare where
  runMessage msg (BlackjackOHare attrs) = BlackjackOHare <$> runMessage msg attrs
