module Marvel.Event.Events.SuitUp where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades
import Data.HashMap.Strict qualified as HashMap

suitUp :: EventCard SuitUp
suitUp = event SuitUp Cards.suitUp

newtype SuitUp = SuitUp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- RR v1.8 erratum: the upgrade can attach to AN ally, independently of the ally
-- found. Search eligibility describes printed attachment types, not whether an
-- ally currently in play has a free upgrade slot or the required trait.
allyAttachment :: PlayerCard -> Bool
allyAttachment card = any (`sameCard` getCardDef card) attachmentDefs
 where
  attachmentDefs = mapMaybe (`HashMap.lookup` Upgrades.allUpgrades)
    ["01074", "03025", "03031", "04015", "12017", "12018", "17019", "18030", "22032", "22035", "23035", "30029", "31029", "32013", "32043", "33015", "33035", "34007", "34016", "42032", "45014", "45015", "45176"]

instance RunMessage SuitUp where
  runMessage msg a@(SuitUp attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> a <$ cardSpecific attrs "ally" ()
    CardSpecific (isTarget attrs -> True) step _ | step == "ally" || step == "upgrade" -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let eligible = if step == "ally" then cardMatch (CardWithType AllyType) else allyAttachment
          candidates = filter eligible (deck <> discard)
          publicCandidates = filter eligible discard
          next = if step == "ally" then "upgrade" else "finish"
      chooseOne you $ [labeled "$label.done" (cardSpecific attrs next ()) | null publicCandidates] <>
        [targeting card (addToHand you card >> cardSpecific attrs next ()) | card <- candidates]
      pure a
    CardSpecific (isTarget attrs -> True) "finish" _ -> a <$ shuffleDeck you
    _ -> SuitUp <$> runMessage msg attrs
   where
    you = eventController attrs
