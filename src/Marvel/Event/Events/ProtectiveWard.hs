module Marvel.Event.Events.ProtectiveWard (
  protectiveWard,
  ProtectiveWard (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Window qualified as W

protectiveWard :: EventCard ProtectiveWard
protectiveWard = event ProtectiveWard Cards.protectiveWard

newtype ProtectiveWard = ProtectiveWard (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ProtectiveWard where
  runMessage msg e@(ProtectiveWard attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck)) ->
        do
          cancelMatchingMessage $ \case
            TreacheryMessage tid' (RevealTreachery _) -> tid' == tid
            _ -> False
          pure e
      _ -> ProtectiveWard <$> runMessage msg attrs
    _ -> ProtectiveWard <$> runMessage msg attrs
