module Marvel.Event.Events.WebTrap where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Damage
import Marvel.Query

webTrap :: EventCard WebTrap
webTrap = event WebTrap Cards.webTrap

newtype WebTrap = WebTrap (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WebTrap where
  runMessage msg e@(WebTrap attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamage enemy attrs (toDamage 5 $ FromPlayerAttack you)
        when (syncRatioResourcesPaid payment > 0) $ cardSpecific attrs "stun" enemy
        | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "stun" (fromJSON -> Success enemy) -> do
      whenM (enemyMatches AnyEnemy enemy) $ stun enemy attrs
      pure e
    _ -> WebTrap <$> runMessage msg attrs
