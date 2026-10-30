module Marvel.Event.Events.ParentalGuidance where

import Marvel.Prelude
import Marvel.Card
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id (unEventId)
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand, PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Supports

parentalGuidance :: EventCard ParentalGuidance
parentalGuidance = event ParentalGuidance Cards.parentalGuidance

newtype ParentalGuidance = ParentalGuidance (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent ParentalGuidance where
  eventTargets (ParentalGuidance attrs) = do
    georges <- selectList (SupportNamed "George Stacy")
    case georges of
      [] -> pure Nothing
      sid : _ -> do
        held <- getSupportHeldCardsOn sid
        let you = eventController attrs
        cards <- (<>) <$> projectMap PlayerIdentityHand unHand you <*> projectMap PlayerIdentityDiscard unDiscard you
        let available = any (\c -> pcCardId c /= CardId (unEventId $ eventId attrs) && cardMatch (CardWithType EventType) c) cards
        pure $ if length held < 3 && available then Nothing else Just []

instance RunMessage ParentalGuidance where
  runMessage msg e@(ParentalGuidance attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      georges <- selectList (SupportNamed "George Stacy")
      case georges of
        sid : _ -> do
          held <- getSupportHeldCardsOn sid
          when (length held < 3) do
            cards <- (<>) <$> projectMap PlayerIdentityHand unHand you <*> projectMap PlayerIdentityDiscard unDiscard you
            let events = filter (cardMatch $ CardWithType EventType) cards
            unless (null events) $ chooseOne you [targeting card (attachPlayableEventToSupport sid card) | card <- events]
        [] -> search you (SearchIdentityDeckAndDiscardPile you) (CardIs Supports.georgeStacy) SearchDrawOne ShuffleBackIn
      pure e
    _ -> ParentalGuidance <$> runMessage msg attrs
