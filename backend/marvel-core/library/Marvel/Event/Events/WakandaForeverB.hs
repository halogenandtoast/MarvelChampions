module Marvel.Event.Events.WakandaForeverB
  ( wakandaForeverB
  , WakandaForeverB(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Target

wakandaForeverB :: EventCard WakandaForeverB
wakandaForeverB = event WakandaForeverB Cards.wakandaForeverB

newtype WakandaForeverB = WakandaForeverB EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage WakandaForeverB where
  runMessage msg e@(WakandaForeverB attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ _ -> do
        pure e
      _ -> WakandaForeverB <$> runMessage msg attrs
    _ -> WakandaForeverB <$> runMessage msg attrs
