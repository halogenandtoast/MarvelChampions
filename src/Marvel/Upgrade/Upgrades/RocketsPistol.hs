module Marvel.Upgrade.Upgrades.RocketsPistol (
  rocketsPistol,
  RocketsPistol (..),
) where

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

-- | 3 charge counters, and no uses keyword: it stays in play with none (FAQ).
rocketsPistol :: UpgradeCard RocketsPistol
rocketsPistol = upgradeWith RocketsPistol Cards.rocketsPistol (usesL .~ 3)

newtype RocketsPistol = RocketsPistol (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Exhaust Rocket's Pistol and remove 1 charge counter from it →
-- deal 2 damage to an enemy.
instance HasAbilities RocketsPistol where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack) (ExhaustCost <> UseCost) $
          runAbility a 1
    ]

instance RunMessage RocketsPistol where
  runMessage msg u@(RocketsPistol attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      enemies <- selectList $ AttackableBy (IdentityWithId ident)
      chooseOne ident
        [ targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 2 $ FromPlayerAttack ident)
        | enemy <- enemies
        ]
      pure u
    _ -> RocketsPistol <$> runMessage msg attrs
