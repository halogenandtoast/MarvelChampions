module Marvel.Event.Events.SuperdenseStrike where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question ()
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W ()

superdenseStrike :: EventCard SuperdenseStrike
superdenseStrike = event SuperdenseStrike Cards.superdenseStrike

newtype SuperdenseStrike = SuperdenseStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SuperdenseStrike where
  runMessage msg e@(SuperdenseStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (withPiercing $ toDamage 5 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> SuperdenseStrike <$> runMessage msg attrs
