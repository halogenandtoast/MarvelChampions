module Marvel.Event.Events.ShadowcatSurprise where

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
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

shadowcatSurprise :: EventCard ShadowcatSurprise
shadowcatSurprise = event ShadowcatSurprise Cards.shadowcatSurprise

newtype ShadowcatSurprise = ShadowcatSurprise (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ShadowcatSurprise where
  runMessage msg e@(ShadowcatSurprise attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
      ready you
      pure e
    _ -> ShadowcatSurprise <$> runMessage msg attrs
