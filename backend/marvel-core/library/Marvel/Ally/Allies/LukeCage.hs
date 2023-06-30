module Marvel.Ally.Allies.LukeCage (
  lukeCage,
  LukeCage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

lukeCage :: AllyCard LukeCage
lukeCage =
  allyWith LukeCage Cards.lukeCage (Thw 1, 1) (Atk 2, 1) (HP 5) (toughL .~ True)

newtype LukeCage = LukeCage (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LukeCage where
  runMessage msg (LukeCage attrs) = LukeCage <$> runMessage msg attrs
