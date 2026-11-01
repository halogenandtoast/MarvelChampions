module Marvel.Event.Events.PhasedStrike where

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
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades

phasedStrike :: EventCard PhasedStrike
phasedStrike = event PhasedStrike Cards.phasedStrike

newtype PhasedStrike = PhasedStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PhasedStrike where
  runMessage msg e@(PhasedStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs (toDamage 6 $ FromPlayerAttack you) >> cardSpecific attrs "attachment" (you, enemy)) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "attachment" (fromJSON -> Success (you, enemy)) -> do
      phased <- isInMassForm you Upgrades.phased
      when phased $ do
        attachments <- selectList (AttachmentOnEnemyWithHeroAbility enemy)
        chooseOne you $ labeled "$label.done" (pure ()) : [targeting aid (removeFromPlay aid) | aid <- attachments]
      pure e
    _ -> PhasedStrike <$> runMessage msg attrs
