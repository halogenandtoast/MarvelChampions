module Marvel.Event.Events.StrengthInNumbers (
  strengthInNumbers,
  StrengthInNumbers (..),
) where

import Marvel.Prelude

import Marvel.Payment
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

strengthInNumbers :: EventCard StrengthInNumbers
strengthInNumbers = event StrengthInNumbers Cards.strengthInNumbers

newtype StrengthInNumbers = StrengthInNumbers (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Exhausting any number of allies is its cost → draw a card for each.
instance RunMessage StrengthInNumbers where
  runMessage msg e@(StrengthInNumbers attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId payment _) | ident == eventId attrs -> do
      let n = fromIntegral $ length $ paymentExhaustedAllies payment
      when (n > 0) $ drawCards identityId n
      pure e
    _ -> StrengthInNumbers <$> runMessage msg attrs
