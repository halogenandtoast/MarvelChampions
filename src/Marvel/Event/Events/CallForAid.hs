module Marvel.Event.Events.CallForAid (
  callForAid,
  CallForAid (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Card
import Marvel.Trait

callForAid :: EventCard CallForAid
callForAid = event CallForAid Cards.callForAid

newtype CallForAid = CallForAid (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Discard cards from the top of your deck until you discard an Avenger ally, then add
-- that ally to your hand.
instance RunMessage CallForAid where
  runMessage msg e@(CallForAid attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      discardFromDeckUntil you (CardWithType AllyType <> CardWithTrait Avenger) attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) _ (PlayerCard card) -> do
      addToHand (eventController attrs) card
      pure e
    _ -> CallForAid <$> runMessage msg attrs
