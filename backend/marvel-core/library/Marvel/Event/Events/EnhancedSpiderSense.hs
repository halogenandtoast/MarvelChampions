module Marvel.Event.Events.EnhancedSpiderSense
  ( enhancedSpiderSense
  , EnhancedSpiderSense(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W

enhancedSpiderSense :: EventCard EnhancedSpiderSense
enhancedSpiderSense = event EnhancedSpiderSense Cards.enhancedSpiderSense

newtype EnhancedSpiderSense = EnhancedSpiderSense EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage EnhancedSpiderSense where
  runMessage msg e@(EnhancedSpiderSense attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _ _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck)) -> do
        cancelMatchingMessage $ \case
          TreacheryMessage tid' (RevealTreachery _) -> tid' == tid
          _ -> False
        pure e
      _ -> EnhancedSpiderSense <$> runMessage msg attrs
    _ -> EnhancedSpiderSense <$> runMessage msg attrs
