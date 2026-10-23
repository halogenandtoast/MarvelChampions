module Marvel.Upgrade.Upgrades.GiantStrength (
  giantStrength,
  giantStrengthEffect,
  GiantStrength (..),
  GiantStrengthEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

giantStrength :: UpgradeCard GiantStrength
giantStrength = upgrade GiantStrength Cards.giantStrength

newtype GiantStrength = GiantStrength (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you change to Giant hero form, you get +1 ATK until the end of
-- this turn.
instance HasAbilities GiantStrength where
  getAbilities (GiantStrength a) =
    [ limitedWindowAbility a 1 (IdentityChangedToForm After (IdentityWithId you <> IdentityWithTrait Giant)) HeroResponse OwnsThis NoCost $
        CreateEffect Cards.giantStrength (toSource a) (TargetMatches $ IdentityEntity $ IdentityWithId you)
    ]
   where
    you = upgradeController a

instance RunMessage GiantStrength where
  runMessage msg (GiantStrength attrs) = GiantStrength <$> runMessage msg attrs

newtype GiantStrengthEffect = GiantStrengthEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

giantStrengthEffect :: CardEffect GiantStrengthEffect
giantStrengthEffect =
  effectWith GiantStrengthEffect Cards.giantStrength $
    (modifiersL .~ [AttackModifier 1]) . (endsL ?~ DisableAtEndOfTurn)

instance RunMessage GiantStrengthEffect where
  runMessage msg (GiantStrengthEffect attrs) = GiantStrengthEffect <$> runMessage msg attrs
