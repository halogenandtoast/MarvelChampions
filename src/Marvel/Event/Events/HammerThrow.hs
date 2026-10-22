module Marvel.Event.Events.HammerThrow (
  hammerThrow,
  HammerThrow (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades

hammerThrow :: EventCard HammerThrow
hammerThrow = event HammerThrow Cards.hammerThrow

newtype HammerThrow = HammerThrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Mjolnir is exhausted as its cost, then 8 damage with overkill, then Mjolnir goes
-- back to hand.
instance RunMessage HammerThrow where
  runMessage msg e@(HammerThrow attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      mjolnir <- selectList $ UpgradeIs Upgrades.mjolnir <> upgradeControlledBy identityId
      enemies <- selectList AttackableEnemy
      pushAll $
        Ask identityId (ChooseOne $ map (damageChoice attrs (withOverkill $ toDamage 8 $ FromPlayerAttack identityId)) enemies)
          : map (ReturnToHand . toTarget) (take 1 mjolnir)
      pure e
    _ -> HammerThrow <$> runMessage msg attrs
