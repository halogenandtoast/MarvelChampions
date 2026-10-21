module Marvel.Minion.Minions.Drone (
  drone,
  Drone (..),
)
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

-- | A player card put into play facedown as a Drone minion. The Ultron Drones
-- environment gives it a base SCH, ATK and hit points of 1.
drone :: MinionCard Drone
drone = minion Drone Cards.drone (Sch 1) (Atk 1) (HP 1)

newtype Drone = Drone (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Drone where
  runMessage msg (Drone attrs) =
    Drone <$> runMessage msg attrs
