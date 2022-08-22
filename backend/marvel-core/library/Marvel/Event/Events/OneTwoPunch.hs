module Marvel.Event.Events.OneTwoPunch
  ( oneTwoPunch
  , OneTwoPunch(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event OneTwoPunch Cards.oneTwoPunch

newtype OneTwoPunch = OneTwoPunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        push $ IdentityMessage identityId ReadiedIdentity
        pure e
      _ -> OneTwoPunch <$> runMessage msg attrs
    _ -> OneTwoPunch <$> runMessage msg attrs
