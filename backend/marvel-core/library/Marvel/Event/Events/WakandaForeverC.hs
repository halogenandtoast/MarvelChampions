module Marvel.Event.Events.WakandaForeverC
  ( wakandaForeverC
  , WakandaForeverC(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Target

wakandaForeverC :: EventCard WakandaForeverC
wakandaForeverC = event WakandaForeverC Cards.wakandaForeverC

newtype WakandaForeverC = WakandaForeverC EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage WakandaForeverC where
  runMessage msg e@(WakandaForeverC attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ _ -> do
        pure e
      _ -> WakandaForeverC <$> runMessage msg attrs
    _ -> WakandaForeverC <$> runMessage msg attrs
