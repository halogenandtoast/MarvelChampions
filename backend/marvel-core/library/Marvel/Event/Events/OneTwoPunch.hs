module Marvel.Event.Events.OneTwoPunch (
  oneTwoPunch,
  OneTwoPunch (..),
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

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event OneTwoPunch Cards.oneTwoPunch

newtype OneTwoPunch = OneTwoPunch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        push $ IdentityMessage identityId ReadiedIdentity
        pure e
      _ -> OneTwoPunch <$> runMessage msg attrs
    _ -> OneTwoPunch <$> runMessage msg attrs
