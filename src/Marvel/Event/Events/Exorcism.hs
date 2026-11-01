module Marvel.Event.Events.Exorcism where

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
import Marvel.Deck
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Projection
import Marvel.Resource.Types

exorcism :: EventCard Exorcism
exorcism = event Exorcism Cards.exorcism

newtype Exorcism = Exorcism (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Exorcism where
  runMessage msg e@(Exorcism attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      removeThreat you attrs 4 ThwartableScheme
      cardSpecific attrs "confuse" you
      pure e
    CardSpecific (isTarget attrs -> True) "confuse" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      when (maybe False (any (`elem` [Mental, Wild]) . printedResources . getCardDef) (headMay deck)) $
        withTheVillain you (\villain -> confuse villain attrs)
      pure e
    _ -> Exorcism <$> runMessage msg attrs
