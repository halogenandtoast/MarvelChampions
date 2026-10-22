module Marvel.Event.Events.AncestralKnowledge (
  ancestralKnowledge,
  AncestralKnowledge (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier

import Marvel.Ref

ancestralKnowledge :: EventCard AncestralKnowledge
ancestralKnowledge = event AncestralKnowledge Cards.ancestralKnowledge

newtype AncestralKnowledge = AncestralKnowledge (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage AncestralKnowledge where
  runMessage msg e@(AncestralKnowledge attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        chooseFromDiscard identityId attrs DifferentCards 0 3
        pure e
      _ -> AncestralKnowledge <$> runMessage msg attrs
    WithChosen (isTarget e -> True) _ (onlyPlayerCards -> cards) -> do
      shuffleIntoDeck (eventController attrs) cards
      pure e
    _ -> AncestralKnowledge <$> runMessage msg attrs
