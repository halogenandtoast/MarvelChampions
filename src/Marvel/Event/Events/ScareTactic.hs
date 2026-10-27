module Marvel.Event.Events.ScareTactic (
  scareTactic,
  ScareTactic (..),
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

scareTactic :: EventCard ScareTactic
scareTactic = event ScareTactic Cards.scareTactic

newtype ScareTactic = ScareTactic (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 3 damage to a confused enemy.
instance RunMessage ScareTactic where
  runMessage msg e@(ScareTactic attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList $ AttackableEnemy <> ConfusedEnemy
      chooseOne you
        [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> ScareTactic <$> runMessage msg attrs
