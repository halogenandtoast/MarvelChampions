module Marvel.Ally.Allies.WarMachine (
  warMachine,
  WarMachine (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost ()
import Marvel.Criteria ()

warMachine :: AllyCard WarMachine
warMachine = ally WarMachine Cards.warMachine (Thw 2, 1) (Atk 2, 1) (HP 3)

-- | Toughness, and a ranged basic attack: his keywords.
newtype WarMachine = WarMachine (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage WarMachine where
  runMessage msg (WarMachine attrs) = WarMachine <$> runMessage msg attrs
