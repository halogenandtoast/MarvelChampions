module Marvel.Event.Events.CombatReady (combatReady, CombatReady (..)) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Hero.Heroes.NebulaHero (resolveTechnique)
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query ()
import Marvel.Ref
import Marvel.Trait (Trait (Technique))

combatReady :: EventCard CombatReady
combatReady = event CombatReady Cards.combatReady

newtype CombatReady = CombatReady (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

techniqueCard :: PlayerCard -> Bool
techniqueCard = cardMatch (CardWithType UpgradeType <> CardWithTrait Technique)

instance IsEvent CombatReady where
  eventTargets (CombatReady attrs) = do
    let ident = eventController attrs
    discards <- projectMap PlayerIdentityDiscard unDiscard ident
    deck <- projectMap PlayerIdentityDeck unDeck ident
    -- Finding no Technique still discards the deck, a meaningful effect. Finding
    -- one puts it into play even when its Special cannot subsequently resolve.
    pure $ if any techniqueCard discards || not (null deck) then Nothing else Just []

instance RunMessage CombatReady where
  runMessage msg e@(CombatReady attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent ident _ _) | eid == eventId attrs -> do
      discards <- projectMap PlayerIdentityDiscard unDiscard ident
      deck <- projectMap PlayerIdentityDeck unDeck ident
      let recover = any techniqueCard discards
      unless (not recover && null deck) $ chooseOne ident $
        [labeled "$label.combatReadyShuffle" (cardSpecific attrs "combatReadyShuffle" ([] :: [PlayerCard])) | recover]
          <> [labeled "$label.combatReadyDiscard" (discardFromDeckUntil ident (CardWithType UpgradeType <> CardWithTrait Technique) attrs) | not (null deck)]
      pure e
    CardSpecific (isTarget attrs -> True) "combatReadyShuffle" (fromJSON -> Success selected) -> do
      discards <- filter techniqueCard <$> projectMap PlayerIdentityDiscard unDiscard ident
      let candidates = filter (`notElem` selected) discards
      if length selected >= 2 || null candidates
        then unless (null selected) $ shuffleIntoDeck ident selected
        else chooseOne ident $
          labeled "$label.done" (unless (null selected) $ shuffleIntoDeck ident selected)
            : [targeting card (cardSpecific attrs "combatReadyShuffle" (selected <> [card])) | card <- candidates]
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromDeck (PlayerCard card) -> do
      putIntoPlay ident card
      -- The upgrade must exist before its Special is inspected or resolved.
      cardSpecific attrs "combatReadySpecial" (UpgradeId $ unCardId $ pcCardId card)
      pure e
    CardSpecific (isTarget attrs -> True) "combatReadySpecial" (fromJSON -> Success uid) -> do
      resolveTechnique ident uid
      pure e
    _ -> CombatReady <$> runMessage msg attrs
   where ident = eventController attrs
