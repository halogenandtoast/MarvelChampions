module Marvel.Event.Events.Emergency (
  emergency,
  Emergency (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window

emergency :: EventCard Emergency
emergency = event Emergency Cards.emergency

newtype Emergency = Emergency (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Emergency where
  runMessage msg e@(Emergency attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ (Just (ThreatPlaced _ schemeId n)) -> do
        replaceMatchingMessage
          (const [threatPlacement schemeId (subtractNatural 1 n)])
          (isThreatPlacement schemeId)
        pure e
      _ -> Emergency <$> runMessage msg attrs
    _ -> Emergency <$> runMessage msg attrs
