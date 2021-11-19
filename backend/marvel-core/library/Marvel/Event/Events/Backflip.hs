module Marvel.Event.Events.Backflip
  ( Backflip
  , backflip
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

backflip :: EventCard Backflip
backflip = event Backflip Cards.backflip

newtype Backflip = Backflip EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Backflip where
  runMessage msg e@(Backflip attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        cancelMatchingMessage $ \case
          IdentityMessage identityId' (IdentityDamaged _ _) ->
            identityId' == identityId
          _ -> False
        pure e
      _ -> Backflip <$> runMessage msg attrs
    _ -> Backflip <$> runMessage msg attrs
