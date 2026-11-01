module Marvel.Event.Events.TelekineticAttack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
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
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

telekineticAttack :: EventCard TelekineticAttack
telekineticAttack = event TelekineticAttack Cards.telekineticAttack

newtype TelekineticAttack = TelekineticAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TelekineticAttack where
  runMessage msg e@(TelekineticAttack a) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId a -> do
      unleashed <- identityMatches (IdentityWithTrait Trait.Unleashed) you
      chooseDamage you a ((toDamage (if unleashed then 9 else 7) $ FromPlayerAttack you) {damageOverkill = unleashed}) AttackableEnemy
      pure e
    _ -> TelekineticAttack <$> runMessage msg a
