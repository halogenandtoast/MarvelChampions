module Marvel.Event.Events.DropKick (
  dropKick,
  DropKick (..),
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

dropKick :: EventCard DropKick
dropKick = event DropKick Cards.dropKick

newtype DropKick = DropKick (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 4 damage to an enemy; paid for using only [physical] resources, stun that
-- enemy and draw 1 card.
instance RunMessage DropKick where
  runMessage msg e@(DropKick attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidUsingOnly Physical payment
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ targeting (enemyRef enemy) do
              dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack identityId)
              when physical do
                stun enemy attrs
                drawCards identityId 1
          | enemy <- enemies
          ]
        pure e
      _ -> DropKick <$> runMessage msg attrs
    _ -> DropKick <$> runMessage msg attrs
