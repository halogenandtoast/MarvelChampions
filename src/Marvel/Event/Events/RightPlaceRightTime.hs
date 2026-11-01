module Marvel.Event.Events.RightPlaceRightTime where
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

rightPlaceRightTime :: EventCard RightPlaceRightTime
rightPlaceRightTime = event (RightPlaceRightTime . (`With` (Nothing :: Maybe SchemeId))) Cards.rightPlaceRightTime
newtype RightPlaceRightTime = RightPlaceRightTime (Attrs Event `With` Maybe SchemeId)
  deriving newtype (Show, Eq, ToJSON, FromJSON)


instance IsEvent RightPlaceRightTime where
  toEventAttrs (RightPlaceRightTime (attrs `With` _)) = attrs
instance IsRef RightPlaceRightTime where
  toRef = toRef . toEventAttrs
instance HasModifiersFor RightPlaceRightTime

instance RunMessage RightPlaceRightTime where
  runMessage msg a@(RightPlaceRightTime (attrs `With` remembered)) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (cardSpecific attrs "first" scheme) | scheme <- schemes]
      pure a
    CardSpecific (isTarget attrs -> True) "first" (fromJSON -> Success scheme) -> do
      discardFromDeck (eventController attrs) 1 attrs
      pure $ RightPlaceRightTime (attrs `With` Just scheme)
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      n <- discardedResourceCount (eventController attrs) cards
      for_ remembered $ \scheme -> thwartSchemeAs (eventController attrs) scheme attrs (3 + n)
      pure a
    _ -> RightPlaceRightTime . (`With` remembered) <$> runMessage msg attrs
