module Marvel.Event.Events.PredictablePloy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

predictablePloy :: EventCard PredictablePloy
predictablePloy = event PredictablePloy Cards.predictablePloy

newtype PredictablePloy = PredictablePloy (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PredictablePloy where
  runMessage msg a@(PredictablePloy attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck))) | eid == eventId attrs -> a <$ cancelWhenRevealed tid
    _ -> PredictablePloy <$> runMessage msg attrs
