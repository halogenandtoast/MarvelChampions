module Marvel.Event.Events.EnhancedSpiderSense (
  enhancedSpiderSense,
  EnhancedSpiderSense (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

enhancedSpiderSense :: EventCard EnhancedSpiderSense
enhancedSpiderSense = event EnhancedSpiderSense Cards.enhancedSpiderSense

newtype EnhancedSpiderSense = EnhancedSpiderSense (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage EnhancedSpiderSense where
  runMessage msg e@(EnhancedSpiderSense attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck)) -> do
        cancelWhenRevealed tid
        pure e
      _ -> EnhancedSpiderSense <$> runMessage msg attrs
    _ -> EnhancedSpiderSense <$> runMessage msg attrs
