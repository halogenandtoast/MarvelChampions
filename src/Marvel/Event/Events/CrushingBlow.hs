module Marvel.Event.Events.CrushingBlow (
  crushingBlow,
  CrushingBlow (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

crushingBlow :: EventCard CrushingBlow
crushingBlow = event CrushingBlow Cards.crushingBlow

newtype CrushingBlow = CrushingBlow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Paid for with only [physical] resources (see its criteria): deal damage equal to
-- your ATK.
instance RunMessage CrushingBlow where
  runMessage msg e@(CrushingBlow attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        atk <- selectCount HeroAttackDamage (IdentityWithId identityId)
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage atk $ FromPlayerAttack identityId) | enemy <- enemies]
        pure e
      _ -> CrushingBlow <$> runMessage msg attrs
    _ -> CrushingBlow <$> runMessage msg attrs
