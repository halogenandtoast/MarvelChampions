module Marvel.Event.Events.JustPassingThrough where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

justPassingThrough :: EventCard JustPassingThrough
justPassingThrough = event JustPassingThrough Cards.justPassingThrough

newtype JustPassingThrough = JustPassingThrough (Attrs Event)
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor JustPassingThrough where
  getModifiersFor _ (EventRef eid) (JustPassingThrough a) | eid == eventId a = pure [IgnoresPatrol]
  getModifiersFor _ _ _ = pure []

instance RunMessage JustPassingThrough where
  runMessage msg e@(JustPassingThrough attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      removeThreat you attrs 3 ThwartableSchemeIgnoringCrisis
      pure e
    _ -> JustPassingThrough <$> runMessage msg attrs
