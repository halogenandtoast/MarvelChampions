module Marvel.Event.Events.SurpriseAttack (
  surpriseAttack,
  SurpriseAttack (..),
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

surpriseAttack :: EventCard SurpriseAttack
surpriseAttack = event SurpriseAttack Cards.surpriseAttack

newtype SurpriseAttack = SurpriseAttack (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response (attack): After you change form, deal 3 damage to an enemy (4 damage
-- instead if you paid for this card using a [physical] resource).
instance RunMessage SurpriseAttack where
  runMessage msg e@(SurpriseAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      physical <- paidWith Physical payment
      enemies <- selectList AttackableEnemy
      chooseOne you
        [ targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage (if physical then 4 else 3) $ FromPlayerAttack you)
        | enemy <- enemies
        ]
      pure e
    _ -> SurpriseAttack <$> runMessage msg attrs
