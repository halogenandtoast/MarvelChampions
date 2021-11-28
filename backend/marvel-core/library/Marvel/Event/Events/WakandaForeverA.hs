module Marvel.Event.Events.WakandaForeverA
  ( wakandaForeverA
  , WakandaForeverA(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Target

wakandaForeverA :: EventCard WakandaForeverA
wakandaForeverA = event WakandaForeverA Cards.wakandaForeverA

newtype WakandaForeverA = WakandaForeverA EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage WakandaForeverA where
  runMessage msg e@(WakandaForeverA attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ _ -> do
        pure e
      _ -> WakandaForeverA <$> runMessage msg attrs
    _ -> WakandaForeverA <$> runMessage msg attrs
