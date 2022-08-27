module Marvel.Event.Events.ForJustice
  ( forJustice
  , ForJustice(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

forJustice :: EventCard ForJustice
forJustice = event ForJustice Cards.forJustice

newtype ForJustice = ForJustice (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage ForJustice where
  runMessage msg e@(ForJustice attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payments _ -> do
        resources <- paymentResources payments
        let
          usedEnergy = Energy `elem` resources || Wild `elem` resources
          thwart = if usedEnergy then 4 else 3
        pushAll =<< choiceMessages
          identityId
          (RemoveThreat (toSource attrs) thwart ThwartableScheme)
        pure e
      _ -> ForJustice <$> runMessage msg attrs
    _ -> ForJustice <$> runMessage msg attrs
