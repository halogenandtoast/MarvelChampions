module Marvel.Ally.Allies.Charlie27 (
  charlie27,
  Charlie27 (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

charlie27 :: AllyCard Charlie27
charlie27 = ally Charlie27 Cards.charlie27 (Thw 1, 1) (Atk 1, 1) (HP 4)

-- | Retaliate 1 and Toughness: his keywords.
newtype Charlie27 = Charlie27 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Charlie27 where
  runMessage msg (Charlie27 attrs) = Charlie27 <$> runMessage msg attrs
