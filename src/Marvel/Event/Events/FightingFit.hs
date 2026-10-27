module Marvel.Event.Events.FightingFit (
  fightingFit,
  FightingFit (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getRemainingHitPoints)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

fightingFit :: EventCard FightingFit
fightingFit = event FightingFit Cards.fightingFit

newtype FightingFit = FightingFit (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Deal 2 damage to the villain (5 damage instead if your hero's
-- remaining hit points are equal to or greater than your hero's starting hit points, its
-- printed hit points).
instance RunMessage FightingFit where
  runMessage msg e@(FightingFit attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      (remaining, _, printed) <- getRemainingHitPoints you
      let n = if remaining >= printed then 5 else 2
      chooseDamage you attrs (toDamage n $ FromPlayerAttack you) (VillainEnemy <> AttackableEnemy)
      pure e
    _ -> FightingFit <$> runMessage msg attrs
