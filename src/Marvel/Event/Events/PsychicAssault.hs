module Marvel.Event.Events.PsychicAssault where

import Marvel.Prelude
import Marvel.Ability
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
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psychicAssault :: EventCard PsychicAssault
psychicAssault = event PsychicAssault Cards.psychicAssault

newtype PsychicAssault = PsychicAssault (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsychicAssault where
  runMessage msg e@(PsychicAssault attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you) >> giveStatus enemy ConfusedStatus) | enemy <- enemies]
      pure e
    _ -> PsychicAssault <$> runMessage msg attrs
