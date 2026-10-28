module Marvel.Minion.Minions.AllyMinion (
  allyMinion,
  AllyMinion (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | An ally treated as a minion with a blank text box (Fallen Warrior, Beguiled): no
-- ability of its own. Its card, stats and traits are the ally's ('allyAsMinion'); what
-- it gains is its attachment's.
allyMinion :: MinionCard AllyMinion
allyMinion = minion AllyMinion Cards.allyMinion (Sch 0) (Atk 0) (HP 0)

newtype AllyMinion = AllyMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AllyMinion where
  runMessage msg (AllyMinion attrs) = AllyMinion <$> runMessage msg attrs
