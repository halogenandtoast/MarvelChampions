module Marvel.Event.Events.YoungLove where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Query
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

youngLove :: EventCard YoungLove
youngLove = event YoungLove Cards.youngLove

newtype YoungLove = YoungLove (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage YoungLove where
  runMessage msg a@(YoungLove attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      for_ ["Gwen Stacy", "Miles Morales"] \name -> do
        traverse_ (`heal` 3) =<< selectList (IdentityWithTitle name)
        traverse_ (`heal` 3) =<< selectList (AllyNamedAs name)
      pure a
    _ -> YoungLove <$> runMessage msg attrs
