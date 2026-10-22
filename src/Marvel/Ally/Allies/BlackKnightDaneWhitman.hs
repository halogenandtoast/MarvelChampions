module Marvel.Ally.Allies.BlackKnightDaneWhitman (
  blackKnightDaneWhitman,
  BlackKnightDaneWhitman (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria

blackKnightDaneWhitman :: AllyCard BlackKnightDaneWhitman
blackKnightDaneWhitman = ally BlackKnightDaneWhitman Cards.blackKnightDaneWhitman (Thw 1, 1) (Atk 2, 1) (HP 3)

-- | His basic attack's piercing is his keyword, read as he attacks.
newtype BlackKnightDaneWhitman = BlackKnightDaneWhitman (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage BlackKnightDaneWhitman where
  runMessage msg (BlackKnightDaneWhitman attrs) = BlackKnightDaneWhitman <$> runMessage msg attrs
