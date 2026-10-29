module Marvel.Upgrade.Upgrades.ShoulderCannon (shoulderCannon, ShoulderCannon (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

shoulderCannon :: UpgradeCard ShoulderCannon
shoulderCannon = upgrade ShoulderCannon Cards.shoulderCannon

newtype ShoulderCannon = ShoulderCannon (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ShoulderCannon where
  getAbilities a = [subtype Ability.Attack $
    ability a 1 HeroAction (OwnsThis <> canAttack) ExhaustCost (runAbility a 1)]

instance RunMessage ShoulderCannon where
  runMessage msg u@(ShoulderCannon attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      enemies <- selectList $ AttackableBy (IdentityWithId ident)
      unless (null enemies) $ chooseOne ident
        [ targeting (enemyRef enemy) do
            dealDamageThen enemy attrs (toDamage 1 $ FromPlayerAttack ident) $
              cardSpecific attrs "reload" ident
        | enemy <- enemies ]
      pure u
    CardSpecific (isTarget attrs -> True) "reload" (fromJSON -> Success ident) -> do
      ammo <- project PlayerIdentityCounters ident
      when (ammo > 0) $ chooseOne ident
        [ labeled "$label.readyShoulderCannon" (removeCounters ident 1 >> ready (upgradeId attrs))
        , labeled "$label.done" (pure ()) ]
      pure u
    _ -> ShoulderCannon <$> runMessage msg attrs
