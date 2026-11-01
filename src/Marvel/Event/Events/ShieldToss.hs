module Marvel.Event.Events.ShieldToss (
  shieldToss,
  ShieldToss (..),
) where

import Marvel.Prelude

import Marvel.Payment
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards ()

shieldToss :: EventCard ShieldToss
shieldToss = event ShieldToss Cards.shieldToss

newtype ShieldToss = ShieldToss (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | X cards are discarded and the shield goes back to hand as its cost, then each of X
-- enemies is dealt 4 damage.
instance RunMessage ShieldToss where
  runMessage msg e@(ShieldToss attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ payment _) | ident == eventId attrs -> do
      when (notNull $ paymentDiscards payment) $
        cardSpecific attrs "toss" (length (paymentDiscards payment), [] :: [EnemyId])
      pure e
    -- the X enemies are chosen, then dealt their damage at once
    CardSpecific (isTarget attrs -> True) "toss" (fromJSON -> Success (x :: Int, chosen :: [EnemyId])) -> do
      let identityId = eventController attrs
      enemies <- filter (`notElem` chosen) <$> selectList AttackableEnemy
      if length chosen >= x || null enemies
        then dealDamageToEach (reverse chosen) attrs (toDamage 4 $ FromPlayerAttack identityId)
        else
          chooseOne identityId
            [targeting enemy $ cardSpecific attrs "toss" (x, enemy : chosen) | enemy <- enemies]
      pure e
    _ -> ShieldToss <$> runMessage msg attrs
