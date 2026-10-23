module Marvel.Upgrade.Upgrades.WristGauntlets (
  wristGauntlets,
  WristGauntlets (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

wristGauntlets :: UpgradeCard WristGauntlets
wristGauntlets = upgrade WristGauntlets Cards.wristGauntlets

newtype WristGauntlets = WristGauntlets (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: If you are in Giant hero form, exhaust Wrist Gauntlets and spend
-- [physical][physical] resources → stun an enemy.
-- Hero Action: If you are in Tiny hero form, exhaust Wrist Gauntlets and spend
-- [energy][energy] resources → confuse an enemy.
instance HasAbilities WristGauntlets where
  getAbilities (WristGauntlets a) =
    [ ability a 1 HeroAction (inForm Giant) (ExhaustCost <> MultiResourceCost [Just Physical, Just Physical]) $ runAbility a 1
    , ability a 2 HeroAction (inForm Tiny) (ExhaustCost <> MultiResourceCost [Just Energy, Just Energy]) $ runAbility a 2
    ]
   where
    inForm t = OwnsThis <> SelfMatches (IdentityWithTrait t) <> EnemyExists AnyEnemy

instance RunMessage WristGauntlets where
  runMessage msg u@(WristGauntlets attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) n _ _ -> do
      enemies <- selectList AnyEnemy
      chooseOne you
        [ targeting (enemyRef enemy) (if n == 1 then stun enemy attrs else confuse enemy attrs)
        | enemy <- enemies
        ]
      pure u
    _ -> WristGauntlets <$> runMessage msg attrs
