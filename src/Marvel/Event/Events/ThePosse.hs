module Marvel.Event.Events.ThePosse where
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

thePosse :: EventCard ThePosse
thePosse = event ThePosse Cards.thePosse
newtype ThePosse = ThePosse (Attrs Event)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent ThePosse where
  eventTargets (ThePosse attrs) = do
    characters <- selectList (CharacterWithTrait Trait.Posse <> CharacterControlledBy (IdentityWithId $ eventController attrs))
    pure $ if length characters >= 3 then Nothing else Just []

instance RunMessage ThePosse where
  runMessage msg a@(ThePosse attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      posse <- selectList (CharacterWithTrait Trait.Posse)
      for_ posse $ \character -> do
        heal character 1
        case character of
          IdentityCharacter ident -> ready ident
          AllyCharacter aid -> ready aid
          _ -> pure ()
      pure a
    _ -> ThePosse <$> runMessage msg attrs
