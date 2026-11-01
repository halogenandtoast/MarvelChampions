module Marvel.Event.Events.ArcticAttack where

import Marvel.Prelude
import Marvel.Ability ()
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
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.Event.Types

arcticAttack :: EventCard ArcticAttack
arcticAttack = event ArcticAttack Cards.arcticAttack

newtype ArcticAttack = ArcticAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ArcticAttack where
  runMessage msg a@(ArcticAttack attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      frozen <- selectList (AttackableEnemy <> frostbitten)
      chooseOne you $
        [labeled "$label.arcticAttackFreeze" $ chooseOne you [targeting (enemyRef enemy) $ attachFrostbiteAndDamage you enemy attrs (toDamage 4 $ FromPlayerAttack you) | enemy <- enemies] | notNull enemies]
        <> [labeled "$label.arcticAttackDamage" $ chooseOne you [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 6 $ FromPlayerAttack you) | enemy <- frozen] | notNull frozen]
      pure a
    _ -> ArcticAttack <$> runMessage msg attrs
