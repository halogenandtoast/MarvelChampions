module Marvel.Event.Events.TeamInvestigation where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

teamInvestigation :: EventCard TeamInvestigation
teamInvestigation = event TeamInvestigation Cards.teamInvestigation
newtype TeamInvestigation = TeamInvestigation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TeamInvestigation where
  runMessage msg a@(TeamInvestigation attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      n <- getStartingPlayerCount
      removeThreat you attrs (3 * fromIntegral n) (NotScheme MainScheme)
      pure a
    _ -> TeamInvestigation <$> runMessage msg attrs
