module Marvel.Event.Events.EnergyTransfer where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Hero.Heroes.RogueHero (copyRogueTraits)
import Marvel.Payment

energyTransfer :: EventCard EnergyTransfer
energyTransfer = event EnergyTransfer Cards.energyTransfer

newtype EnergyTransfer = EnergyTransfer (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage EnergyTransfer where
  runMessage msg e@(EnergyTransfer attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      for_ [host | TouchedPayment host _ <- payments payment] $ \host -> do
        heal you 2
        ready you
        copyRogueTraits attrs you host
      pure e
    _ -> EnergyTransfer <$> runMessage msg attrs
