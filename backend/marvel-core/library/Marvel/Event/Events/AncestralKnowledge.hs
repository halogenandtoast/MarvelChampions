module Marvel.Event.Events.AncestralKnowledge
  ( ancestralKnowledge
  , AncestralKnowledge(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Target

ancestralKnowledge :: EventCard AncestralKnowledge
ancestralKnowledge = event AncestralKnowledge Cards.ancestralKnowledge

newtype AncestralKnowledge = AncestralKnowledge EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage AncestralKnowledge where
  runMessage msg e@(AncestralKnowledge attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ _ -> do
        pure e
      _ -> AncestralKnowledge <$> runMessage msg attrs
    _ -> AncestralKnowledge <$> runMessage msg attrs
