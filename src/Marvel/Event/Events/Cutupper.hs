module Marvel.Event.Events.Cutupper where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W ()

cutupper :: EventCard Cutupper
cutupper = event Cutupper Cards.cutupper

newtype Cutupper = Cutupper (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cutupper where
  runMessage msg e@(Cutupper attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) $
        dealDamageThen enemy attrs (toDamage 5 $ FromPlayerAttack you) (stun enemy attrs)
        | enemy <- enemies]
      pure e
    _ -> Cutupper <$> runMessage msg attrs
