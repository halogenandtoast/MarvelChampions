module Marvel.Upgrade.Upgrades.FlowLikeWater where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

flowLikeWater :: UpgradeCard FlowLikeWater
flowLikeWater = upgrade FlowLikeWater Cards.flowLikeWater

newtype FlowLikeWater = FlowLikeWater (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FlowLikeWater where
  getAbilities a@(FlowLikeWater attrs) =
    [ limitedWindowAbility a 1 (W.PlaysEvent W.After (IdentityWithId $ upgradeController attrs) $ CardWithTrait Trait.Defense)
        Response (OwnsThis <> EnemyExists AttackingEnemy) NoCost (runAbility a 1)
    ]

instance RunMessage FlowLikeWater where
  runMessage msg u@(FlowLikeWater attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      enemy <- selectOne AttackingEnemy
      for_ enemy \attacker -> dealDamage attacker attrs (toDamage 1 FromAbility)
      pure u
    _ -> FlowLikeWater <$> runMessage msg attrs
