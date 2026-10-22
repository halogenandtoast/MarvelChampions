module Marvel.Event.Events.Thunderclap (
  thunderclap,
  Thunderclap (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

thunderclap :: EventCard Thunderclap
thunderclap = event Thunderclap Cards.thunderclap

newtype Thunderclap = Thunderclap (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Choose up to 3 different enemies. Deal 3 damage to each of them": the enemies are
-- chosen one at a time, then each is dealt its damage.
instance RunMessage Thunderclap where
  runMessage msg e@(Thunderclap attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent {} -> do
        cardSpecific attrs "chosen" ([] :: [EnemyId])
        pure e
      _ -> Thunderclap <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "chosen" (fromJSON -> Success chosen) -> do
      let
        player = eventController attrs
        -- the chosen are dealt their damage at once
        deal = dealDamageToEach (reverse chosen) attrs (toDamage 3 FromAbility)
      enemies <- filter (`notElem` chosen) <$> selectList DamageableEnemy
      if length chosen >= 3 || null enemies
        then deal
        else
          chooseOne player $
            [labeled "$label.done" deal | notNull chosen]
              <> [targeting (enemyRef enemy) $ cardSpecific attrs "chosen" (enemy : chosen) | enemy <- enemies]
      pure e
    _ -> Thunderclap <$> runMessage msg attrs
