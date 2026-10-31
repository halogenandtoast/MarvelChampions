module Marvel.Event.Events.PsychicManipulation where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psychicManipulation :: EventCard PsychicManipulation
psychicManipulation = event PsychicManipulation Cards.psychicManipulation

newtype PsychicManipulation = PsychicManipulation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsychicManipulation where
  runMessage msg e@(PsychicManipulation attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.Schemed vid))) | eid == eventId attrs ->
      do
        deferEventResolutionUntilScheme eid vid
        villainSchemeRemovesThreat vid you attrs
        pure e
    _ -> PsychicManipulation <$> runMessage msg attrs
