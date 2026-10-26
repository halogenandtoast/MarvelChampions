module Marvel.Upgrade.Upgrades.LieInWait (
  lieInWait,
  LieInWait (..),
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
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

lieInWait :: UpgradeCard LieInWait
lieInWait = upgrade LieInWait Cards.lieInWait

newtype LieInWait = LieInWait (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response (attack): After a minion engages you, discard Lie in Wait → deal 3
-- damage to that minion.
instance HasAbilities LieInWait where
  getAbilities a =
    [ subtype Ability.Attack $
        limitedWindowAbility
          a
          1
          (EngagesMinion After (IdentityWithId $ upgradeController $ toUpgradeAttrs a) AnyMinion)
          HeroResponse
          OwnsThis
          (DiscardCost $ toTarget a)
          (runAbility a 1)
    ]

instance RunMessage LieInWait where
  runMessage msg u@(LieInWait attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 windows _ ->
      attackGuard ident u do
        for_ [minion | EngagedMinion _ minion <- windows] \minion ->
          dealDamage minion attrs (toDamage 3 $ FromPlayerAttack ident)
        pure u
    _ -> LieInWait <$> runMessage msg attrs
