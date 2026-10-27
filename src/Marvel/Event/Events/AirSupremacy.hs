module Marvel.Event.Events.AirSupremacy (
  airSupremacy,
  AirSupremacy (..),
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
import Marvel.Trait

airSupremacy :: EventCard AirSupremacy
airSupremacy = event AirSupremacy Cards.airSupremacy

newtype AirSupremacy = AirSupremacy (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Choose up to X enemies, where X is equal to the number of Aerial characters you
-- control → deal 3 damage to each chosen enemy": X counted as it is played, the enemies
-- chosen one at a time (as Thunderclap), then each dealt its damage at once.
instance RunMessage AirSupremacy where
  runMessage msg e@(AirSupremacy attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        x <- length <$> selectList (CharacterControlledBy (IdentityWithId you) <> CharacterWithTrait Aerial)
        cardSpecific attrs "chosen" (x, [] :: [EnemyId])
        pure e
      _ -> AirSupremacy <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "chosen" (fromJSON -> Success (x :: Int, chosen)) -> do
      let deal = dealDamageToEach (reverse chosen) attrs (toDamage 3 FromAbility)
      enemies <- filter (`notElem` chosen) <$> selectList DamageableEnemy
      if length chosen >= x || null enemies
        then deal
        else
          chooseOne (eventController attrs) $
            labeled "$label.done" deal
              : [targeting (enemyRef enemy) $ cardSpecific attrs "chosen" (x, enemy : chosen) | enemy <- enemies]
      pure e
    _ -> AirSupremacy <$> runMessage msg attrs
