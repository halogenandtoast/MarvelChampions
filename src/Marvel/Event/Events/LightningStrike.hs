module Marvel.Event.Events.LightningStrike (
  lightningStrike,
  LightningStrike (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
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

-- | Spend X [energy] resources (a wild resource counts), with its resource cost → X
-- damage to the villain and each minion engaged with you, past tough status cards if
-- you are Aerial. Not an attack (FAQ errata).
instance RunMessage LightningStrike where
  runMessage msg e@(LightningStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId payment _) | ident == eventId attrs -> do
      let x = paymentX payment
      aerial <- selectAny $ IdentityWithId identityId <> IdentityWithTrait Aerial
      minions <- selectList $ MinionEngagedWith (IdentityWithId identityId)
      let damage = (if aerial then ignoringTough else id) $ toDamage x FromAbility
      when (x > 0) $ withTheVillainIfAny identityId \villain ->
        dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs damage
      pure e
    _ -> LightningStrike <$> runMessage msg attrs
