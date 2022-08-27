module Marvel.Minion.Minions.Vulture
  ( vulture
  , Vulture(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

vulture :: MinionCard Vulture
vulture = minion Vulture Cards.vulture (Sch 1) (Atk 3) (HP 4)

newtype Vulture = Vulture (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage Vulture where
  runMessage msg (Vulture attrs) = Vulture <$> runMessage msg attrs
