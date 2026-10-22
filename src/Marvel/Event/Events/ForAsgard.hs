module Marvel.Event.Events.ForAsgard (
  forAsgard,
  ForAsgard (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait

forAsgard :: EventCard ForAsgard
forAsgard = event ForAsgard Cards.forAsgard

newtype ForAsgard = ForAsgard (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ForAsgard where
  runMessage msg e@(ForAsgard attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        push . IdentityMessage identityId $
          Search (SearchIdentityDeckAndDiscardPile identityId) (CardWithTrait Asgard) SearchDrawOne ShuffleBackIn
        pure e
      _ -> ForAsgard <$> runMessage msg attrs
    _ -> ForAsgard <$> runMessage msg attrs
