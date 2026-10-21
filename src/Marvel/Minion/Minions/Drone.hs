module Marvel.Minion.Minions.Drone
  ( drone
  , Drone(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Minion.Runner
import Marvel.Minion.Cards qualified as Cards

drone :: MinionCard Drone
drone = minion Drone Cards.drone (Sch 0) (Atk 0) (HP 1)

newtype Drone = Drone (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Drone where
  runMessage msg (Drone attrs) =
    Drone <$> runMessage msg attrs
