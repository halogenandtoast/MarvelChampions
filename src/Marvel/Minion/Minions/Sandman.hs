module Marvel.Minion.Minions.Sandman (
  sandman,
  Sandman (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

sandman :: MinionCard Sandman
sandman =
  minionWith Sandman Cards.sandman (Sch 2) (Atk 3) (HP 4) (toughL .~ True)

newtype Sandman = Sandman (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Sandman where
  runMessage msg (Sandman attrs) = Sandman <$> runMessage msg attrs
