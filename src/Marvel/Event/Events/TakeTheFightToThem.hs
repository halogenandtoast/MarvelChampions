module Marvel.Event.Events.TakeTheFightToThem (
  takeTheFightToThem,
  TakeTheFightToThem (..),
) where

import Marvel.Prelude

import Data.List (delete)
import Marvel.Card
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getPlayerCount)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

takeTheFightToThem :: EventCard TakeTheFightToThem
takeTheFightToThem = event TakeTheFightToThem Cards.takeTheFightToThem

newtype TakeTheFightToThem = TakeTheFightToThem (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Where the cards looked at are going: those for the top (the first topmost), those
-- for the bottom (the last bottommost), and those not yet placed.
data Placing = Placing {onTop :: [EncounterCard], onBottom :: [EncounterCard], unplaced :: [EncounterCard]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Hero Action: look at the top 2 per player cards of the encounter deck; discard any
-- of them, the rest going on the top and/or bottom in any order. A card at a time is
-- chosen and discarded, or put on the top beneath those put there already, or on the
-- bottom beneath those put there already. Then draw 1 card.
instance RunMessage TakeTheFightToThem where
  runMessage msg e@(TakeTheFightToThem attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      n <- getPlayerCount
      lookAtTopOfEncounterDeck (fromIntegral $ 2 * n) attrs
      pure e
    SearchFoundCards (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      lookingAt you (EncounterCard <$> cards) $ cardSpecific attrs "placing" (Placing [] [] cards)
      drawCards you 1
      pure e
    CardSpecific (isTarget attrs -> True) "placing" (fromJSON -> Success p) -> do
      case unplaced p of
        [] -> do
          putOnTopOfEncounterDeck (onTop p)
          putOnBottomOfEncounterDeck (onBottom p)
        cards ->
          chooseOne you [targeting c (cardSpecific attrs "card" (c, p {unplaced = delete c cards})) | c <- cards]
      pure e
    CardSpecific (isTarget attrs -> True) "card" (fromJSON -> Success (c, p)) -> do
      let next = cardSpecific attrs "placing"
      chooseOne you
        [ labeled "$label.discardIt" (discardCard (EncounterCard c) >> next p)
        , labeled "$label.putOnTopOfEncounterDeck" (next p {onTop = onTop p <> [c]})
        , labeled "$label.putOnBottomOfEncounterDeck" (next p {onBottom = onBottom p <> [c]})
        ]
      pure e
    _ -> TakeTheFightToThem <$> runMessage msg attrs
   where
    you = eventController attrs
