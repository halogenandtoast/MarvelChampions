module Marvel.Event.Events.WingIt (
  wingIt,
  WingIt (..),
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

wingIt :: EventCard WingIt
wingIt = event WingIt Cards.wingIt

newtype WingIt = WingIt (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: 1 damage to an enemy and confuse it (one effect), confuse your hero,
-- draw 1.
instance RunMessage WingIt where
  runMessage msg e@(WingIt attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AnyEnemy
      chooseOne you
        [ targeting (enemyRef enemy) $ dealDamageAnd enemy attrs (toDamage 1 FromAbility) (confuse enemy attrs)
        | enemy <- enemies
        ]
      confuseIdentity you
      drawCards you 1
      pure e
    _ -> WingIt <$> runMessage msg attrs
