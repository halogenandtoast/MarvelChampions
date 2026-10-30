module Marvel.Minion.Minions.TheLizard where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Phase (Phase (VillainPhase))

theLizard :: MinionCard TheLizard
theLizard = minion TheLizard Cards.theLizard (Sch 1) (Atk 3) (HP 5)

newtype TheLizard = TheLizard (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheLizard where
  runMessage msg m@(TheLizard attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> m <$ heal (minionId attrs) 1
    _ -> TheLizard <$> runMessage msg attrs
