module Marvel.Upgrade.Upgrades.DeadpoolsKatana where

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
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

deadpoolsKatana :: UpgradeCard DeadpoolsKatana
deadpoolsKatana = upgrade DeadpoolsKatana Cards.deadpoolsKatana

newtype DeadpoolsKatana = DeadpoolsKatana (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DeadpoolsKatana where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack) (ExhaustCost <> DamageCost 1) (runAbility a 1)
    ]

instance RunMessage DeadpoolsKatana where
  runMessage msg u@(DeadpoolsKatana attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> attackGuard you u do
      enemies <- selectList $ AttackableBy (IdentityWithId you)
      unless (null enemies) $ chooseOne you
        [ targeting (enemyRef enemy) $ dealDamage enemy attrs (withPiercing $ toDamage 2 $ FromPlayerAttack you)
        | enemy <- enemies
        ]
      pure u
    _ -> DeadpoolsKatana <$> runMessage msg attrs
