module Marvel.Event.Events.Crosscounter (
  crosscounter,
  Crosscounter (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

crosscounter :: EventCard Crosscounter
crosscounter = event Crosscounter Cards.crosscounter

newtype Crosscounter = Crosscounter (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When you would take damage: prevent 3 of it, deal 1 damage to an enemy, remove 1
-- threat from a scheme. Labeled attack, defense and thwart: a stun or confusion cancels
-- all of it (the engine's 'ResolveEventEffect').
instance RunMessage Crosscounter where
  runMessage msg e@(Crosscounter attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        preventDamage identityId 3
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 1 $ FromPlayerAttack identityId) | enemy <- enemies]
        removeThreat identityId attrs 1 ThwartableScheme
        pure e
      _ -> Crosscounter <$> runMessage msg attrs
    _ -> Crosscounter <$> runMessage msg attrs
