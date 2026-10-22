module Marvel.Event.Events.LegalPractice (
  legalPractice,
  LegalPractice (..),
) where

import Marvel.Prelude

import Marvel.Payment
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

legalPractice :: EventCard LegalPractice
legalPractice = event LegalPractice Cards.legalPractice

newtype LegalPractice = LegalPractice (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discarding up to 5 cards is its cost → remove 1 threat for each.
instance RunMessage LegalPractice where
  runMessage msg e@(LegalPractice attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId payment _) | ident == eventId attrs -> do
      removeThreat identityId attrs (fromIntegral $ length $ paymentDiscards payment) ThwartableScheme
      pure e
    _ -> LegalPractice <$> runMessage msg attrs
