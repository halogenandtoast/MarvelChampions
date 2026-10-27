module Marvel.Event.Events.HitAndRun (
  hitAndRun,
  HitAndRun (..),
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
import Marvel.Ref

hitAndRun :: EventCard HitAndRun
hitAndRun = event HitAndRun Cards.hitAndRun

newtype HitAndRun = HitAndRun (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 2 damage to an enemy. Remove 2 threat from a scheme.
instance RunMessage HitAndRun where
  runMessage msg e@(HitAndRun attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 2 $ FromPlayerAttack you) AttackableEnemy
      removeThreat you attrs 2 ThwartableScheme
      pure e
    _ -> HitAndRun <$> runMessage msg attrs
