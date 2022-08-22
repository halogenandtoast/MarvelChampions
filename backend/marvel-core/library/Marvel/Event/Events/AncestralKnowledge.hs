module Marvel.Event.Events.AncestralKnowledge
  ( ancestralKnowledge
  , AncestralKnowledge(..)
  ) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target

ancestralKnowledge :: EventCard AncestralKnowledge
ancestralKnowledge = event AncestralKnowledge Cards.ancestralKnowledge

newtype AncestralKnowledge = AncestralKnowledge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

instance RunMessage AncestralKnowledge where
  runMessage msg e@(AncestralKnowledge attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        push
          (IdentityMessage identityId
          $ ChooseFromDiscard (toTarget attrs) DifferentCards 0 3
          )
        pure e
      _ -> AncestralKnowledge <$> runMessage msg attrs
    WithChosen (isTarget e -> True) _ (onlyPlayerCards -> cards) -> do
      push $ IdentityMessage
        (eventController attrs)
        (ShuffleIntoIdentityDeck cards)
      pure e
    _ -> AncestralKnowledge <$> runMessage msg attrs
