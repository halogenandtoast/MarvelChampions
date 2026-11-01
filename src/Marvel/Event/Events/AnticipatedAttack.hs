module Marvel.Event.Events.AnticipatedAttack where

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
import Marvel.Status

anticipatedAttack :: EventCard AnticipatedAttack
anticipatedAttack = event AnticipatedAttack Cards.anticipatedAttack

newtype AnticipatedAttack = AnticipatedAttack (Attrs Event)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent AnticipatedAttack where
  eventTargets (AnticipatedAttack attrs) = do
    canGain <- selectAny (CharacterWithId (IdentityCharacter $ eventController attrs) <> CharacterCanGainTough)
    pure $ if canGain then Nothing else Just []

instance RunMessage AnticipatedAttack where
  runMessage msg a@(AnticipatedAttack attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> a <$ giveStatus you ToughStatus
    _ -> AnticipatedAttack <$> runMessage msg attrs
