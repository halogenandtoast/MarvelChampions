module Marvel.Event.Events.StopHittingYourselfSilk where
import Marvel.Prelude
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

stopHittingYourselfSilk :: EventCard StopHittingYourselfSilk
stopHittingYourselfSilk = event StopHittingYourselfSilk Cards.stopHittingYourselfSilk
newtype StopHittingYourselfSilk = StopHittingYourselfSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage StopHittingYourselfSilk where
  runMessage msg u@(StopHittingYourselfSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.DefendedWithoutDamage _ enemy))) | eid == eventId attrs -> do
      defense <- getIdentityDefenseForLastAttack you
      dealDamage enemy attrs (toDamage defense $ FromPlayerAttack you)
      pure u
    _ -> StopHittingYourselfSilk <$> runMessage msg attrs
