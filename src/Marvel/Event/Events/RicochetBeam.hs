module Marvel.Event.Events.RicochetBeam where

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
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

ricochetBeam :: EventCard RicochetBeam
ricochetBeam = event RicochetBeam Cards.ricochetBeam

newtype RicochetBeam = RicochetBeam (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RicochetBeam where
  runMessage msg e@(RicochetBeam attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) do
        dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you)
        cardSpecific attrs "secondBeam" you
        | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "secondBeam" (fromJSON -> Success you) -> do
      enemies <- selectList (AttackableBy (IdentityWithId you) <> EnemyWithUpgrade (UpgradeMatches []))
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 3 $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> RicochetBeam <$> runMessage msg attrs
