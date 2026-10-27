module Marvel.Upgrade.Upgrades.ElementGun (
  elementGun,
  ElementGun (..),
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

elementGun :: UpgradeCard ElementGun
elementGun = upgrade ElementGun Cards.elementGun

newtype ElementGun = ElementGun (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Exhaust Element Gun and spend 1 resource of any type → deal 3
-- damage to an enemy. This attack gains piercing.
instance HasAbilities ElementGun where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack) (ExhaustCost <> ResourceCost Nothing) $
          runAbility a 1
    ]

instance RunMessage ElementGun where
  runMessage msg u@(ElementGun attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      enemies <- selectList $ AttackableBy (IdentityWithId ident)
      chooseOne ident
        [ targeting (enemyRef enemy) $ dealDamage enemy attrs (withPiercing $ toDamage 3 $ FromPlayerAttack ident)
        | enemy <- enemies
        ]
      pure u
    _ -> ElementGun <$> runMessage msg attrs
