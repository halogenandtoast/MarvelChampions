module Marvel.Event.Events.MagicAttack (
  magicAttack,
  MagicAttack (..),
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

magicAttack :: EventCard MagicAttack
magicAttack = event MagicAttack Cards.magicAttack

newtype MagicAttack = MagicAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Choose an enemy and discard up to 5 cards from the top of your
-- deck (its cost) → deal 1 damage to that enemy for each card discarded this way, as one
-- instance.
instance RunMessage MagicAttack where
  runMessage msg e@(MagicAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      let x = fromIntegral $ length (paymentDiscards payment)
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage x $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> MagicAttack <$> runMessage msg attrs
