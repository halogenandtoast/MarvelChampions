module Marvel.Event.Events.Defiance where

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

defiance :: EventCard Defiance
defiance = event Defiance Cards.defiance

newtype Defiance = Defiance (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Defiance where
  runMessage msg e@(Defiance attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.FlippedBoostCard card _))) | eid == eventId attrs -> do
      discardBoostCardBeforeReveal card
      pure e
    _ -> Defiance <$> runMessage msg attrs
