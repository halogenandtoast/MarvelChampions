module Marvel.Event.Events.CounterPunch
  ( counterPunch
  , CounterPunch(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window

counterPunch :: EventCard CounterPunch
counterPunch = event CounterPunch Cards.counterPunch

newtype CounterPunch = CounterPunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage CounterPunch where
  runMessage msg e@(CounterPunch attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ (Just (HeroDefends _ enemyId)) -> do
        dmg <- selectCount HeroAttackDamage (IdentityWithId identityId)
        msgs <- choiceMessages
          identityId
          (DamageEnemy
            (EnemyTarget enemyId)
            (toSource attrs)
            (toDamage dmg $ FromPlayerAttack identityId)
          )
        pushAll msgs
        pure e
      _ -> CounterPunch <$> runMessage msg attrs
    _ -> CounterPunch <$> runMessage msg attrs
