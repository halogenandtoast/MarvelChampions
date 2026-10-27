module Marvel.Upgrade.Upgrades.ParticleCannon (
  particleCannon,
  ParticleCannon (..),
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

-- | 2 charge counters, and no uses keyword: it stays in play with none (FAQ).
particleCannon :: UpgradeCard ParticleCannon
particleCannon = upgradeWith ParticleCannon Cards.particleCannon (usesL .~ 2)

newtype ParticleCannon = ParticleCannon (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Exhaust Particle Cannon and remove 1 charge counter from it →
-- deal 4 damage to an enemy. This attack gains overkill and ranged.
instance HasAbilities ParticleCannon where
  getAbilities a =
    [ subtype Ability.Attack $
        ability a 1 HeroAction (OwnsThis <> canAttack) (ExhaustCost <> UseCost) $
          runAbility a 1
    ]

instance RunMessage ParticleCannon where
  runMessage msg u@(ParticleCannon attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> attackGuard ident u do
      enemies <- selectList $ AttackableBy (IdentityWithId ident)
      chooseOne ident
        [ targeting (enemyRef enemy) $
            dealDamage enemy attrs (withOverkill $ withRanged $ toDamage 4 $ FromPlayerAttack ident)
        | enemy <- enemies
        ]
      pure u
    _ -> ParticleCannon <$> runMessage msg attrs
