module Marvel.Event.Events.Grapple (
  grapple,
  Grapple (..),
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

grapple :: EventCard Grapple
grapple = event Grapple Cards.grapple

newtype Grapple = Grapple (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: 1 damage to an enemy and stun it (one effect), stun your hero, draw 1.
instance RunMessage Grapple where
  runMessage msg e@(Grapple attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AnyEnemy
      chooseOne you
        [ targeting (enemyRef enemy) $ dealDamageAnd enemy attrs (toDamage 1 FromAbility) (stun enemy attrs)
        | enemy <- enemies
        ]
      stunIdentity you
      drawCards you 1
      pure e
    _ -> Grapple <$> runMessage msg attrs
