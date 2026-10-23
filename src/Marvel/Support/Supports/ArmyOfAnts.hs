module Marvel.Support.Supports.ArmyOfAnts (
  armyOfAnts,
  ArmyOfAnts (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait

armyOfAnts :: SupportCard ArmyOfAnts
armyOfAnts = support ArmyOfAnts Cards.armyOfAnts

newtype ArmyOfAnts = ArmyOfAnts (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: If you are in Tiny hero form, exhaust Army of Ants → deal 1 damage to an
-- enemy.
instance HasAbilities ArmyOfAnts where
  getAbilities a =
    [ ability a 1 HeroAction (OwnsThis <> SelfMatches (IdentityWithTrait Tiny) <> EnemyExists AnyEnemy) ExhaustCost $
        ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy
    ]

instance RunMessage ArmyOfAnts where
  runMessage msg (ArmyOfAnts a) = ArmyOfAnts <$> runMessage msg a
