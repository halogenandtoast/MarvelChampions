module Marvel.Event.Events.PsiBowAttack where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Ability.Types qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psiBowAttack :: EventCard PsiBowAttack
psiBowAttack = event PsiBowAttack Cards.psiBowAttack

newtype PsiBowAttack = PsiBowAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsiBowAttack where
  runMessage msg e@(PsiBowAttack a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> e <$ chooseDamage you a (withRanged $ toDamage 4 $ FromPlayerAttack you) AttackableEnemy
    _ -> PsiBowAttack <$> runMessage msg a
