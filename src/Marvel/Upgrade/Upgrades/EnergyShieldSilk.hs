module Marvel.Upgrade.Upgrades.EnergyShieldSilk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import Marvel.Payment
import Marvel.Resource

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

energyShieldSilk :: UpgradeCard EnergyShieldSilk
energyShieldSilk = upgrade EnergyShieldSilk Cards.energyShieldSilk
newtype EnergyShieldSilk = EnergyShieldSilk (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage EnergyShieldSilk where
  runMessage msg u@(EnergyShieldSilk attrs) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just (IdentityRef host))) | uid == upgradeId attrs -> do
      upgradeStep uid (UpgradeControlledByPlayer host)
      upgradeStep uid (UpgradeAttachedToIdentity host)
      pure u
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      n <- fromIntegral . length <$> paymentResources payment
      for_ (upgradeAttachedIdentity attrs) (`preventDamage` n)
      for_ (upgradeAttachedAlly attrs) (`preventAllyDamage` n)
      for_ (upgradeAttachedEnemy attrs) (`preventEnemyDamage` n)
      pure u
    _ -> EnergyShieldSilk <$> runMessage msg attrs

instance HasAbilities EnergyShieldSilk where
  getAbilities u@(EnergyShieldSilk attrs) = [limitedWindowAbility u 1 (W.WindowOneOf $
    [W.WouldTakeDamage (IdentityWithId you) FromAnyDamageSource AnyValue | Just you <- [upgradeAttachedIdentity attrs]] <>
    [W.AllyWouldTakeDamage (AllyWithId aid) | Just aid <- [upgradeAttachedAlly attrs]] <>
    [W.EnemyWouldTakeDamage (EnemyWithId enemy) | Just enemy <- [upgradeAttachedEnemy attrs]])
    Interrupt OwnsThis (DynamicResourceCost $ Just Energy) (runAbility u 1)]

instance IsUpgrade EnergyShieldSilk where
  toUpgradeAttrs (EnergyShieldSilk attrs) = attrs
  upgradeTargets _ = Just . map characterRef <$> selectList AnyCharacter
