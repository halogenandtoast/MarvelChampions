module Marvel.Event.Events.NewAndImproved where

import Marvel.Id (CharacterId (IdentityCharacter))
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card.Def
import Marvel.Deck
import Marvel.Identity (ironheartVersion)
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Status

newAndImproved :: EventCard NewAndImproved
newAndImproved = event NewAndImproved Cards.newAndImproved

newtype NewAndImproved = NewAndImproved (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NewAndImproved where
  runMessage msg e@(NewAndImproved attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      version <- ironheartVersion you
      cardSpecific attrs "options" (version, [] :: [Int])
      pure e
    CardSpecific (isTarget attrs -> True) "options" (fromJSON -> Success (n :: Natural, used :: [Int])) -> do
      let you = eventController attrs
          next k = when (n > 1) $ cardSpecific attrs "options" (n - 1, k : used)
      canReady <- characterMatches CharacterCanBeReadied (IdentityCharacter you)
      tough <- characterMatches (CharacterWithStatus ToughStatus) (IdentityCharacter you)
      -- Select different options; options that cannot change game state are omitted.
      deck <- project PlayerIdentityDeck you
      let searchable = not (null $ unDeck deck)
      let choices =
            [labeled "$label.newAndImprovedSearch" (search you (SearchIdentityDeck you AllOfDeck) HeroSpecificCard SearchDrawOne ShuffleBackIn >> next 1) | 1 `notElem` used, searchable]
            <> [labeled "$label.newAndImprovedTough" (becomeTough you >> next 2) | 2 `notElem` used, not tough]
            <> [labeled "$label.newAndImprovedReady" (ready you >> next 3) | 3 `notElem` used, canReady]
      when (n > 0 && notNull choices) $ chooseOne you choices
      pure e
    _ -> NewAndImproved <$> runMessage msg attrs
