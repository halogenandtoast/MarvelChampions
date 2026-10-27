module Marvel.Support.Supports.TargetPractice (
  targetPractice,
  TargetPractice (..),
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
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait
import Marvel.Window qualified as W

targetPractice :: SupportCard TargetPractice
targetPractice = support TargetPractice Cards.targetPractice

newtype TargetPractice = TargetPractice (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Interrupt: When an ally with a Weapon attachment upgrade makes an attack, discard
-- Target Practice → that ally gets +2 ATK for that attack."
instance HasAbilities TargetPractice where
  getAbilities (TargetPractice a) =
    [ limitedWindowAbility
        a
        1
        (W.AllyAttacked W.When (AllyWithUpgrade $ UpgradeWithTrait Weapon) AnyEnemy)
        Interrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

instance RunMessage TargetPractice where
  runMessage msg c@(TargetPractice attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [W.AllyAttack ally _] _ -> do
      raiseAllyAttack ally 2
      pure c
    _ -> TargetPractice <$> runMessage msg attrs
