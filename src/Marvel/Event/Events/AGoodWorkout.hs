module Marvel.Event.Events.AGoodWorkout where
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

aGoodWorkout :: EventCard AGoodWorkout
aGoodWorkout = event (AGoodWorkout . (`With` (Nothing :: Maybe EnemyId))) Cards.aGoodWorkout
newtype AGoodWorkout = AGoodWorkout (Attrs Event `With` Maybe EnemyId)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEvent AGoodWorkout where
  toEventAttrs (AGoodWorkout (attrs `With` _)) = attrs
instance IsRef AGoodWorkout where
  toRef = toRef . toEventAttrs
instance HasModifiersFor AGoodWorkout
instance HasAbilities AGoodWorkout

instance RunMessage AGoodWorkout where
  runMessage msg a@(AGoodWorkout (attrs `With` remembered)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (cardSpecific attrs "target" enemy) | enemy <- enemies]
      pure a
    CardSpecific (isTarget attrs -> True) "target" (fromJSON -> Success enemy) -> do
      discardFromDeck (eventController attrs) 1 attrs
      pure $ AGoodWorkout (attrs `With` Just enemy)
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let you = eventController attrs
      n <- discardedResourceCount you cards
      for_ remembered $ \enemy -> dealDamage enemy attrs (toDamage (4 + n) $ FromPlayerAttack you)
      pure a
    _ -> AGoodWorkout . (`With` remembered) <$> runMessage msg attrs
