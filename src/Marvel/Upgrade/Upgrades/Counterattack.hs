module Marvel.Upgrade.Upgrades.Counterattack (
  counterattack,
  Counterattack (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

counterattack :: UpgradeCard Counterattack
counterattack = upgrade Counterattack Cards.counterattack

newtype Counterattack = Counterattack (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Counterattack where
  getAbilities a@(Counterattack attrs) =
    [ subtype Ability.Attack $
        limitedWindowAbility
          a
          1
          (TookDamage you FromAttack (AttackableBy you))
          HeroResponse
          OwnsThis
          (DiscardCost $ toTarget a)
          (runAbility a 1)
    ]
   where
    you = IdentityWithId $ upgradeController attrs

-- | The enemy that attacked takes as much damage, as an attack: guard keeps the villain
-- from it, so it is not offered then.
instance RunMessage Counterattack where
  runMessage msg u@(Counterattack attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 [IdentityTookDamage _ source dmg] _ ->
      attackGuard ident u do
        pushChoice ident $
          DamageEnemy (refocus source) (toSource attrs) (toDamage (damageAmount dmg) (FromPlayerAttack ident))
        pure u
    _ -> Counterattack <$> runMessage msg attrs
