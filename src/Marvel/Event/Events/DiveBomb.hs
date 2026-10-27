module Marvel.Event.Events.DiveBomb (
  diveBomb,
  DiveBomb (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Id
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

diveBomb :: EventCard DiveBomb
diveBomb = event DiveBomb Cards.diveBomb

newtype DiveBomb = DiveBomb (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 7 damage to an enemy. Deal 1 damage to each other enemy. A sentence at a time
-- (FAQ): the second sees the game the first left, a guard minion it defeated no guard.
instance RunMessage DiveBomb where
  runMessage msg e@(DiveBomb attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you
        [ targeting (enemyRef enemy) do
            dealDamage enemy attrs (toDamage 7 $ FromPlayerAttack you)
            cardSpecific attrs "others" (you, enemy)
        | enemy <- enemies
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "others" (fromJSON -> Success (you, first :: EnemyId)) -> do
      others <- selectList $ AttackableEnemy <> NotEnemy (EnemyWithId first)
      dealDamageToEach others attrs (toDamage 1 $ FromPlayerAttack you)
      pure e
    _ -> DiveBomb <$> runMessage msg attrs
