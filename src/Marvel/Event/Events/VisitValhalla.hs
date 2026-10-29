module Marvel.Event.Events.VisitValhalla (visitValhalla, VisitValhalla (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

visitValhalla :: EventCard VisitValhalla
visitValhalla = event VisitValhalla Cards.visitValhalla

newtype VisitValhalla = VisitValhalla (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- Only Valkyrie's signature cards, even if another identity plays the event.
valkyrieCards :: CardMatcher
valkyrieCards = HeroSpecificCard <> CardMatchesAny (map CardWithTitle
  ["Death-Glow", "Annabelle Riggs", "Valhalla", "Valkyrie's Spear", "Dragonfang", "Aragorn", "Flight of the Valkyrior", "Visit Valhalla", "Chooser of the Slain", "Shieldmaiden", "Have at Thee!"])

instance RunMessage VisitValhalla where
  runMessage msg e@(VisitValhalla attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches valkyrieCards)
      unless (null cards) $ chooseOne you [targeting card $ addToHand you card | card <- cards]
      pure e
    _ -> VisitValhalla <$> runMessage msg attrs
