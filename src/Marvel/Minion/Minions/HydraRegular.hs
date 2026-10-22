module Marvel.Minion.Minions.HydraRegular (
  hydraRegular,
  HydraRegular (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraRegular :: MinionCard HydraRegular
hydraRegular = minion HydraRegular Cards.hydraRegular (Sch 1) (Atk 2) (HP 2)

-- | Incite 1: its keyword.
newtype HydraRegular = HydraRegular (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HydraRegular where
  runMessage msg (HydraRegular attrs) = HydraRegular <$> runMessage msg attrs
