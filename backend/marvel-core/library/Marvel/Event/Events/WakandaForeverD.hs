module Marvel.Event.Events.WakandaForeverD
  ( wakandaForeverD
  , WakandaForeverD(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Source
import Marvel.Target

wakandaForeverD :: EventCard WakandaForeverD
wakandaForeverD = event WakandaForeverD Cards.wakandaForeverD

newtype WakandaForeverD = WakandaForeverD EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage WakandaForeverD where
  runMessage msg e@(WakandaForeverD attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ _ -> do
        pure e
      _ -> WakandaForeverD <$> runMessage msg attrs
    _ -> WakandaForeverD <$> runMessage msg attrs
