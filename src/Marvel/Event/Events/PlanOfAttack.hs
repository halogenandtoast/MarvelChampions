module Marvel.Event.Events.PlanOfAttack (
  planOfAttack,
  PlanOfAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait

planOfAttack :: EventCard PlanOfAttack
planOfAttack = event PlanOfAttack Cards.planOfAttack

newtype PlanOfAttack = PlanOfAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Search the top 4 cards of your deck (top 7 in alter-ego form) for an attack event
-- and add it to your hand. Shuffle your deck.
instance RunMessage PlanOfAttack where
  runMessage msg e@(PlanOfAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      alterEgo <- identityMatches AlterEgoIdentity you
      search
        you
        (SearchIdentityDeck you $ TopOfDeck $ if alterEgo then 7 else 4)
        (CardWithType EventType <> CardWithTrait Trait.Attack)
        SearchDrawOne
        ShuffleBackIn
      pure e
    _ -> PlanOfAttack <$> runMessage msg attrs
