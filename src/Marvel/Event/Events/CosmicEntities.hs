module Marvel.Event.Events.CosmicEntities (
  inBetweener,
  livingTribunal,
  eternity,
  theGardener,
  CosmicEntity (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

-- | Adam Warlock's Cosmic Entity events, one type: what each does when revealed is told
-- by its card.
newtype CosmicEntity = CosmicEntity (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

inBetweener :: EventCard CosmicEntity
inBetweener = event CosmicEntity Cards.inBetweener

livingTribunal :: EventCard CosmicEntity
livingTribunal = event CosmicEntity Cards.livingTribunal

eternity :: EventCard CosmicEntity
eternity = event CosmicEntity Cards.eternity

theGardener :: EventCard CosmicEntity
theGardener = event CosmicEntity Cards.theGardener

-- | Action: Shuffle this card into the encounter deck (without looking): it goes there,
-- not to the discard pile. When Revealed, for the player revealing it (R8): its effect,
-- then it is removed from the game. This effect cannot be canceled (the reveal opens no
-- window for it); it resolves once (R9).
instance RunMessage CosmicEntity where
  runMessage msg e@(CosmicEntity attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | eid == eventId attrs -> do
      shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unEventId eid) (eventCardDef attrs)]
      pure e
    EventMessage eid (RevealedEvent you) | eid == eventId attrs -> do
      whenRevealed you
      removeFromGame (EventRef eid)
      pure e
    _ -> CosmicEntity <$> runMessage msg attrs
   where
    whenRevealed you
      | is Cards.inBetweener = withTheVillain you \v -> dealDamage v attrs (toDamage 2 FromAbility)
      | is Cards.livingTribunal = removeThreat you attrs 2 MainScheme
      | is Cards.eternity = drawCards you 1
      | is Cards.theGardener = heal (IdentityCharacter you) 2
      | otherwise = pure ()
    is = sameCard (eventCardDef attrs)
