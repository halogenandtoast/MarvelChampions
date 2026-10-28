module Marvel.Event.Events.KarmicBlast (
  karmicBlast,
  KarmicBlast (..),
) where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card
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

karmicBlast :: EventCard KarmicBlast
karmicBlast = event KarmicBlast Cards.karmicBlast

newtype KarmicBlast = KarmicBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 4 damage to an enemy and discard up to 4 cards from the top of your deck (its
-- cost) → 1 additional damage for each different aspect discarded, one instance (R7).
instance RunMessage KarmicBlast where
  runMessage msg e@(KarmicBlast attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you payment _ -> do
        let x = distinctAspects $ map (cdAspect . getCardDef) (paymentDiscards payment)
        enemies <- selectList AttackableEnemy
        chooseOne you
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage (4 + x) $ FromPlayerAttack you) | enemy <- enemies]
        pure e
      _ -> KarmicBlast <$> runMessage msg attrs
    _ -> KarmicBlast <$> runMessage msg attrs
