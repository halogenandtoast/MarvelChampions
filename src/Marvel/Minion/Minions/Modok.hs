module Marvel.Minion.Minions.Modok (
  modok,
  Modok (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

modok :: MinionCard Modok
modok = minion Modok Cards.modok (Sch 2) (Atk 2) (HP 8)

newtype Modok = Modok (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Modok where
  runMessage msg (Modok attrs) = Modok <$> runMessage msg attrs
