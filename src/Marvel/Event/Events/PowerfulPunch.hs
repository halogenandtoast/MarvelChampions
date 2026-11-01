module Marvel.Event.Events.PowerfulPunch where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

powerfulPunch :: EventCard PowerfulPunch
powerfulPunch = event PowerfulPunch Cards.powerfulPunch

newtype PowerfulPunch = PowerfulPunch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PowerfulPunch where
  runMessage msg e@(PowerfulPunch attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
      pure e
    _ -> PowerfulPunch <$> runMessage msg attrs
