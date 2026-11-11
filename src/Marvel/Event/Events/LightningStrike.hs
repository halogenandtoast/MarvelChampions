module Marvel.Event.Events.LightningStrike (
  lightningStrike,
  LightningStrike (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost.Types (Cost (DynamicResourceCost))
import Marvel.Resource.Types (Resource (Energy))
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

lightningStrike :: EventCard LightningStrike
lightningStrike = event LightningStrike Cards.lightningStrike

newtype LightningStrike = LightningStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After paying the printed cost, spend X [energy] resources (wild counts) → X
-- damage to the villain and each minion engaged with you, past tough status cards if
-- you are Aerial. Not an attack (FAQ errata).
instance RunMessage LightningStrike where
  runMessage msg e@(LightningStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      payCostThenWithPayment identityId attrs (DynamicResourceCost $ Just Energy) attrs "lightningDamage" identityId
      pure e
    CardSpecific (isTarget attrs -> True) "lightningDamage" (fromJSON -> Success (identityId, payment)) -> do
      let x = paymentX payment
      aerial <- selectAny $ IdentityWithId identityId <> IdentityWithTrait Aerial
      minions <- selectList $ MinionEngagedWith (IdentityWithId identityId)
      let damage = (if aerial then ignoringTough else id) $ toDamage x FromAbility
      when (x > 0) $ withTheVillainIfAny identityId \villain ->
        dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs damage
      pure e
    _ -> LightningStrike <$> runMessage msg attrs
