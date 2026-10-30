module Marvel.Event.Events.IDontThinkSo where

import Marvel.Prelude
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

iDontThinkSo :: EventCard IDontThinkSo
iDontThinkSo = event IDontThinkSo Cards.iDontThinkSo

newtype IDontThinkSo = IDontThinkSo (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage IDontThinkSo where
  runMessage msg e@(IDontThinkSo attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.EncounterCardRevealed W.RevealedFromEncounterDeck _ card)))
      | eid == eventId attrs -> e <$ cancelEncounterCard card
    _ -> IDontThinkSo <$> runMessage msg attrs
