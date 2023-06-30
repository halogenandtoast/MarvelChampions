module Marvel.Event.Events.LegalPractice (
  legalPractice,
  LegalPractice (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

legalPractice :: EventCard LegalPractice
legalPractice = event LegalPractice Cards.legalPractice

newtype LegalPractice = LegalPractice (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LegalPractice where
  runMessage msg e@(LegalPractice attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        push
          ( IdentityMessage identityId $ DiscardFor (toTarget attrs) FromHand 1 5
          )
        pure e
      _ -> LegalPractice <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      pushAll
        =<< choiceMessages
          (eventController attrs)
          ( RemoveThreat
              (toSource attrs)
              (fromIntegral $ length cards)
              ThwartableScheme
          )
      pure e
    _ -> LegalPractice <$> runMessage msg attrs
