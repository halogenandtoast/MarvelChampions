module Marvel.Minion.Minions.HydraMercenary where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraMercenary :: MinionCard HydraMercenary
hydraMercenary =
  minion HydraMercenary Cards.hydraMercenary (Sch 0) (Atk 1) (HP 3)

newtype HydraMercenary = HydraMercenary (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HydraMercenary where
  runMessage msg (HydraMercenary attrs) =
    HydraMercenary <$> runMessage msg attrs
