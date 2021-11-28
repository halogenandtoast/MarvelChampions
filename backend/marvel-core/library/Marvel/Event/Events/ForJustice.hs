module Marvel.Event.Events.ForJustice
  ( forJustice
  , ForJustice(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

forJustice :: EventCard ForJustice
forJustice = event ForJustice Cards.forJustice

newtype ForJustice = ForJustice EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ForJustice where
  runMessage msg e@(ForJustice attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId payments _ -> do
        let
          resources = paymentResources payments
          usedEnergy = Energy `elem` resources || Wild `elem` resources
          thwart = if usedEnergy then 4 else 3
        pushAll =<< choiceMessages
          identityId
          (RemoveThreat (toSource attrs) thwart ThwartableScheme)
        pure e
      _ -> ForJustice <$> runMessage msg attrs
    _ -> ForJustice <$> runMessage msg attrs
