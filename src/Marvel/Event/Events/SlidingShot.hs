module Marvel.Event.Events.SlidingShot (
  slidingShot,
  SlidingShot (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (PlayerIdentityEncounterCards))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

slidingShot :: EventCard SlidingShot
slidingShot = event SlidingShot Cards.slidingShot

newtype SlidingShot = SlidingShot (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 5 damage to an enemy, and 2 more to it for each facedown encounter card in front
-- of you: one instance of damage. "Play only if you control an Element Gun" is the def's
-- criteria.
instance RunMessage SlidingShot where
  runMessage msg e@(SlidingShot attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        facedown <- length <$> project PlayerIdentityEncounterCards identityId
        chooseDamage identityId attrs (toDamage (5 + 2 * fromIntegral facedown) $ FromPlayerAttack identityId) AttackableEnemy
        pure e
      _ -> SlidingShot <$> runMessage msg attrs
    _ -> SlidingShot <$> runMessage msg attrs
