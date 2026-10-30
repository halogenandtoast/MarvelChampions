module Marvel.Event.Events.GlobalLogistics where

import Marvel.Prelude
import Marvel.Id
import Marvel.Card
import Marvel.Deck
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref

newtype GlobalLogistics = GlobalLogistics (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

globalLogistics :: EventCard GlobalLogistics
globalLogistics = event GlobalLogistics Cards.globalLogistics

instance RunMessage GlobalLogistics where
  runMessage msg e@(GlobalLogistics attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      players <- selectList AnyIdentity
      chooseOne you $
        [targeting player (cardSpecific attrs "look" (you, Just player)) | player <- players]
          <> [labeled "$label.encounterDeck" (cardSpecific attrs "look" (you, Nothing :: Maybe IdentityId))]
      pure e
    CardSpecific (isTarget attrs -> True) "look" (fromJSON -> Success (you, deck)) -> do
      cards <- case deck of
        Just player -> map PlayerCard . take 4 <$> projectMap PlayerIdentityDeck unDeck player
        Nothing -> map EncounterCard . take 4 <$> getEncounterDeck
      showCardsTo you cards
      cardSpecific attrs "arrange" (you, deck, cards, [] :: [Card], [] :: [Card], [] :: [Card])
      pure e
    CardSpecific (isTarget attrs -> True) "arrange" (fromJSON -> Success (you :: IdentityId, deck :: Maybe IdentityId, remaining :: [Card], top :: [Card], bottom :: [Card], discarded :: [Card])) -> do
      if null remaining
        then do
          hideCardsFrom you
          -- Keep the inspected cards in the deck while deciding. Move every kept
          -- card into order first, then discard the selected cards in a single step
          -- so a deck that empties resets only after all selected discards are there.
          case deck of
            Just player -> do
              for_ (reverse [card | PlayerCard card <- top]) (placeOnTopOfDeck player)
              for_ [card | PlayerCard card <- bottom] (placeOnBottomOfDeck player)
              for_ (reverse [card | PlayerCard card <- discarded]) (placeOnTopOfDeck player)
              unless (null discarded) $ discardTopOfDeck player (fromIntegral $ length discarded)
            Nothing -> do
              putOnTopOfEncounterDeck [card | EncounterCard card <- top]
              putOnBottomOfEncounterDeck [card | EncounterCard card <- bottom]
              putOnTopOfEncounterDeck [card | EncounterCard card <- discarded]
              unless (null discarded) $ discardTopOfEncounterDeck (fromIntegral $ length discarded)
        else chooseOne you
          [ targeting (CardIdRef $ toCardId card) $ cardSpecific attrs "destination" (you, deck, card, filter (/= card) remaining, top, bottom, discarded)
          | card <- remaining ]
      pure e
    CardSpecific (isTarget attrs -> True) "destination" (fromJSON -> Success (you :: IdentityId, deck :: Maybe IdentityId, card :: Card, remaining :: [Card], top :: [Card], bottom :: [Card], discarded :: [Card])) -> do
      chooseOne you
        [ labeled "$label.globalLogisticsTop" (cardSpecific attrs "arrange" (you, deck, remaining, top <> [card], bottom, discarded))
        , labeled "$label.globalLogisticsBottom" (cardSpecific attrs "arrange" (you, deck, remaining, top, bottom <> [card], discarded))
        , labeled "$label.globalLogisticsDiscard" (cardSpecific attrs "arrange" (you, deck, remaining, top, bottom, discarded <> [card])) ]
      pure e
    _ -> GlobalLogistics <$> runMessage msg attrs
