module Marvel.Event.Events.HeroicStrike (
  heroicStrike,
  HeroicStrike (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

heroicStrike :: EventCard HeroicStrike
heroicStrike = event HeroicStrike Cards.heroicStrike

newtype HeroicStrike = HeroicStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Paid with a [physical] resource (a wild one is any type), the enemy is stunned too.
instance RunMessage HeroicStrike where
  runMessage msg e@(HeroicStrike attrs) = case msg of
    EventMessage ident (PlayedEvent identityId payments _) | ident == eventId attrs -> do
      resources <- paymentResources payments
      enemies <- selectList AttackableEnemy
      let
        stuns = Physical `elem` resources || Wild `elem` resources
        damage = toDamage 6 $ FromPlayerAttack identityId
      chooseOne identityId
        [ TargetLabel enemy $
            DamageEnemy enemy (toSource attrs) damage : [Stun enemy (toSource attrs) | stuns]
        | enemy <- map toRef enemies
        ]
      pure e
    _ -> HeroicStrike <$> runMessage msg attrs
