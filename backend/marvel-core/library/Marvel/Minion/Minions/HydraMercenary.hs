module Marvel.Minion.Minions.HydraMercenary where

import Marvel.Prelude

import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

hydraMercenary :: MinionCard HydraMercenary
hydraMercenary =
  minion HydraMercenary Cards.hydraMercenary (Sch 0) (Atk 1) (HP 3)

newtype HydraMercenary = HydraMercenary MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HydraMercenary where
  runMessage msg (HydraMercenary attrs) =
    HydraMercenary <$> runMessage msg attrs
