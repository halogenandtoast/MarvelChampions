module Marvel.Upgrade.Upgrades.CivicDuty (
  civicDuty,
  civicDutyEffect,
  CivicDuty (..),
  CivicDutyEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

civicDuty :: UpgradeCard CivicDuty
civicDuty = upgrade CivicDuty Cards.civicDuty

newtype CivicDuty = CivicDuty (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Discard Civic Duty → your hero gets +1 THW until the end of the phase.
instance HasAbilities CivicDuty where
  getAbilities a@(CivicDuty attrs) =
    [ ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget a) $
        CreateEffect Cards.civicDuty (toSource a) (TargetMatches $ IdentityEntity $ IdentityWithId $ upgradeController attrs)
    ]

instance RunMessage CivicDuty where
  runMessage msg (CivicDuty attrs) = CivicDuty <$> runMessage msg attrs

newtype CivicDutyEffect = CivicDutyEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

civicDutyEffect :: CardEffect CivicDutyEffect
civicDutyEffect =
  effectWith CivicDutyEffect Cards.civicDuty $
    (modifiersL .~ [ThwartModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage CivicDutyEffect where
  runMessage msg (CivicDutyEffect attrs) = CivicDutyEffect <$> runMessage msg attrs
