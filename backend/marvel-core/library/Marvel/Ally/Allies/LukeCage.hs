module Marvel.Ally.Allies.LukeCage (
  lukeCage,
  LukeCage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards

lukeCage :: AllyCard LukeCage
lukeCage =
  allyWith LukeCage Cards.lukeCage (Thw 1, 1) (Atk 2, 1) (HP 5) (toughL .~ True)

newtype LukeCage = LukeCage AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage LukeCage where
  runMessage msg a = LukeCage <$> runMessage msg (toAttrs a)
