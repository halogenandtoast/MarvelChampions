module Marvel.Event.Events.BeautyAndTheThief where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Query (selectAny)

beautyAndTheThief :: EventCard BeautyAndTheThief
beautyAndTheThief = event BeautyAndTheThief Cards.beautyAndTheThief

newtype BeautyAndTheThief = BeautyAndTheThief (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BeautyAndTheThief where
  runMessage msg a@(BeautyAndTheThief attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      whenM (selectAny AttackableEnemy) $ chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      cardSpecific attrs "thwart" you
      pure a
    CardSpecific (isTarget attrs -> True) "thwart" (fromJSON -> Success you) -> do
      whenM (selectAny SchemeYouCanThwart) $ removeThreat you attrs 4 SchemeYouCanThwart
      pure a
    _ -> BeautyAndTheThief <$> runMessage msg attrs
