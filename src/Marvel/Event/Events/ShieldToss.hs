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
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards

shieldToss :: EventCard ShieldToss
shieldToss = event ShieldToss Cards.shieldToss

newtype ShieldToss = ShieldToss (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | X cards are discarded and the shield goes back to hand as its cost, then each of X
-- enemies is dealt 4 damage.
instance RunMessage ShieldToss where
  runMessage msg e@(ShieldToss attrs) = case msg of
    EventMessage ident (PlayedEvent identityId payment _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      let
        x = min (length enemies) (length $ paymentDiscards payment)
        damage = toDamage 4 $ FromPlayerAttack identityId
      pushWhen (x > 0) $
        Ask identityId $
          ChooseN (fromIntegral x) [TargetLabel enemy [DamageEnemy enemy (toSource attrs) damage] | enemy <- map toRef enemies]
      pure e
    _ -> ShieldToss <$> runMessage msg attrs
