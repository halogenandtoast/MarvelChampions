module Marvel.Event.Events.Fusillade (
  fusillade,
  Fusillade (..),
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

fusillade :: EventCard Fusillade
fusillade = event Fusillade Cards.fusillade

newtype Fusillade = Fusillade (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Exhausting a Weapon upgrade you control is its cost (the card def's) → deal 5 damage
-- to an enemy.
instance RunMessage Fusillade where
  runMessage msg e@(Fusillade attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you
        [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 5 $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> Fusillade <$> runMessage msg attrs
