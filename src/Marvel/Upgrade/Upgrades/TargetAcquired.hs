module Marvel.Upgrade.Upgrades.TargetAcquired (
  targetAcquired,
  TargetAcquired (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

targetAcquired :: UpgradeCard TargetAcquired
targetAcquired = upgrade TargetAcquired Cards.targetAcquired

newtype TargetAcquired = TargetAcquired (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Only a boost card with a boost ability, as there is none to cancel otherwise.
instance HasAbilities TargetAcquired where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (BoostCardTurnedFaceup After EncounterCardWithBoostAbility)
        HeroResponse
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

-- | The card's boost ability resolves next: it does not.
instance RunMessage TargetAcquired where
  runMessage msg u@(TargetAcquired attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      cancelMatchingMessage \case
        RevealedAsBoost _ _ -> True
        _ -> False
      pure u
    _ -> TargetAcquired <$> runMessage msg attrs
