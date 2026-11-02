module Marvel.Event.Events.ClawedStrikeShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers

import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Event.Types

clawedStrikeShuri :: EventCard ClawedStrikeShuri
clawedStrikeShuri = event ClawedStrikeShuri Cards.clawedStrikeShuri
newtype ClawedStrikeShuri = ClawedStrikeShuri (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ClawedStrikeShuri where
  runMessage msg u@(ClawedStrikeShuri attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      cardSpecific attrs "special" you
      pure u
    CardSpecific (isTarget attrs -> True) "special" (fromJSON -> Success (you :: IdentityId)) -> u <$ choosePantherSpecial you
    _ -> ClawedStrikeShuri <$> runMessage msg attrs
