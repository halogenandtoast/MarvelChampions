module Marvel.Upgrade.Upgrades.AdrenalineRush (
  adrenalineRush,
  adrenalineRushEffect,
  AdrenalineRush (..),
  AdrenalineRushEffect (..),
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

adrenalineRush :: UpgradeCard AdrenalineRush
adrenalineRush = upgrade AdrenalineRush Cards.adrenalineRush

newtype AdrenalineRush = AdrenalineRush (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Discard Adrenaline Rush → your hero gets +1 ATK until the end of the phase.
instance HasAbilities AdrenalineRush where
  getAbilities a@(AdrenalineRush attrs) =
    [ ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget a) $
        CreateEffect Cards.adrenalineRush (toSource a) (TargetMatches $ IdentityEntity $ IdentityWithId $ upgradeController attrs)
    ]

instance RunMessage AdrenalineRush where
  runMessage msg (AdrenalineRush attrs) = AdrenalineRush <$> runMessage msg attrs

newtype AdrenalineRushEffect = AdrenalineRushEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

adrenalineRushEffect :: CardEffect AdrenalineRushEffect
adrenalineRushEffect =
  effectWith AdrenalineRushEffect Cards.adrenalineRush $
    (modifiersL .~ [AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage AdrenalineRushEffect where
  runMessage msg (AdrenalineRushEffect attrs) = AdrenalineRushEffect <$> runMessage msg attrs
