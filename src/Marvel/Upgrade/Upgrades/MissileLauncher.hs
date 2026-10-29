module Marvel.Upgrade.Upgrades.MissileLauncher (missileLauncher, MissileLauncher (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

missileLauncher :: UpgradeCard MissileLauncher
missileLauncher = upgrade MissileLauncher Cards.missileLauncher

newtype MissileLauncher = MissileLauncher (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MissileLauncher where
  getAbilities a = [subtype Ability.Attack $
    ability a 1 HeroAction (OwnsThis <> canAttack) (ExhaustCost <> IdentityCountersCost 1) (runAbility a 1)]

instance RunMessage MissileLauncher where
  runMessage msg u@(MissileLauncher attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      enemies <- selectList $ AttackableBy (IdentityWithId ident)
      unless (null enemies) $ chooseOne ident
        [targeting (enemyRef enemy) $ dealDamage enemy attrs (withRanged $ toDamage 2 $ FromPlayerAttack ident) | enemy <- enemies]
      pure u
    _ -> MissileLauncher <$> runMessage msg attrs
