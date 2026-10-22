module Marvel.Event.Events.HeroicStrike (
  heroicStrike,
  HeroicStrike (..),
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
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

heroicStrike :: EventCard HeroicStrike
heroicStrike = event HeroicStrike Cards.heroicStrike

newtype HeroicStrike = HeroicStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Paid with a [physical] resource (a wild one is any type), the enemy is stunned too.
instance RunMessage HeroicStrike where
  runMessage msg e@(HeroicStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId payments _) | ident == eventId attrs -> do
      stuns <- paidWith Physical payments
      enemies <- selectList AttackableEnemy
      chooseOne identityId
        [ targeting enemy do
            dealDamage enemy attrs (toDamage 6 $ FromPlayerAttack identityId)
            when stuns $ stun enemy attrs
        | enemy <- enemies
        ]
      pure e
    _ -> HeroicStrike <$> runMessage msg attrs
