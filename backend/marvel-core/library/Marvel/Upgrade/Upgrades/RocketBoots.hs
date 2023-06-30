module Marvel.Upgrade.Upgrades.RocketBoots (
  rocketBoots,
  RocketBoots (..),
  rocketBootsEffect,
  RocketBootsEffect,
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
import Marvel.Resource
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

rocketBoots :: UpgradeCard RocketBoots
rocketBoots = upgrade RocketBoots Cards.rocketBoots

newtype RocketBoots = RocketBoots (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RocketBoots where
  getModifiersFor _ (IdentityRef ident) (RocketBoots a)
    | ident == upgradeController a = pure [HitPointModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities RocketBoots where
  getAbilities (RocketBoots a) =
    [ ability
        a
        1
        HeroAction
        OwnsThis
        (ExhaustCost <> ResourceCost (Just Mental))
        $ CreateEffect
          Cards.rocketBoots
          (toSource a)
          (TargetMatches $ IdentityEntity You)
    ]

instance RunMessage RocketBoots where
  runMessage msg (RocketBoots attrs) = RocketBoots <$> runMessage msg attrs

newtype RocketBootsEffect = RocketBootsEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

rocketBootsEffect :: CardEffect RocketBootsEffect
rocketBootsEffect =
  effectWith RocketBootsEffect Cards.rocketBoots $
    (modifiersL .~ [TraitModifier Aerial])
      . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage RocketBootsEffect where
  runMessage msg (RocketBootsEffect attrs) =
    RocketBootsEffect <$> runMessage msg attrs
