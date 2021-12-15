module Marvel.Minion.Minions.Vulture
  ( vulture
  , Vulture(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Attrs
import Marvel.Minion.Cards qualified as Cards

vulture :: MinionCard Vulture
vulture = minion Vulture Cards.vulture (Sch 1) (Atk 3) (HP 4)

newtype Vulture = Vulture MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Vulture where
  runMessage msg (Vulture attrs) = Vulture <$> runMessage msg attrs
