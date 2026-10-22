module Marvel.Event.Events.ShieldToss (
  shieldToss,
  ShieldToss (..),
) where

import Marvel.Prelude

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

-- | X cards are discarded, no more than there are enemies to deal the damage to, then
-- the shield goes back to hand, then each of X enemies is dealt 4 damage.
instance RunMessage ShieldToss where
  runMessage msg e@(ShieldToss attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      handSize <- projectMap PlayerIdentityHand (length . unHand) identityId
      enemies <- selectList AttackableEnemy
      push $
        IdentityMessage identityId $
          DiscardFor (toTarget attrs) FromHand 0 (fromIntegral $ min handSize (length enemies))
      pure e
    WithDiscarded (isTarget attrs -> True) FromHand discarded -> do
      let identityId = eventController attrs
      shields <- selectList $ UpgradeIs Cards.captainAmericasShield <> upgradeControlledBy identityId
      enemies <- selectList AttackableEnemy
      let damage = toDamage 4 $ FromPlayerAttack identityId
      pushAll $
        map (ReturnToHand . toRef) (take 1 shields)
          <> [ Ask identityId $
              ChooseN
                (fromIntegral $ length discarded)
                [TargetLabel enemy [DamageEnemy enemy (toSource attrs) damage] | enemy <- map toRef enemies]
             | notNull discarded
             ]
      pure e
    _ -> ShieldToss <$> runMessage msg attrs
