module Marvel.Event.Events.SteppingDisc where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Matchers
import Marvel.Query

steppingDisc :: EventCard SteppingDisc
steppingDisc = event SteppingDisc Cards.steppingDisc

newtype SteppingDisc = SteppingDisc (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SteppingDisc where
  runMessage msg e@(SteppingDisc attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      ready you
      cardSpecific attrs "return" you
      pure e
    CardSpecific (isTarget attrs -> True) "return" (fromJSON -> Success you) -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $
        CardMatchesAny [CardWithTitle title | title <- ["Colossus", "Limbo", "Magik's Crown", "Soulsword", "Mystical Armor", "Scrying", "Exorcism", "Soul Strike", "Magic Barrier"]])
      let signature card = toCardCode card `elem` ["45031", "45032", "45033", "45034", "45035", "45036", "45038", "45039", "45040"]
      chooseOne you [targeting card (placeOnTopOfDeck you card) | card <- cards, signature card]
      pure e
    _ -> SteppingDisc <$> runMessage msg attrs
