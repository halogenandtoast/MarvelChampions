module Marvel.Ally.Allies.Vision
  ( vision
  , visionEffect
  , Vision(..)
  , VisionEffect(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Resource

vision :: AllyCard Vision
vision = ally Vision Cards.vision (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Vision = Vision (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities Vision where
  getAbilities a@(Vision attrs) =
    [ limitedAbility
          a
          1
          (PerRound 1)
          Action
          OwnsThis
          (ResourceCost $ Just Energy)
        $ CreateEffect
            Cards.vision
            (toSource a)
            (TargetMatches $ AllyEntity $ AllyWithId $ allyId attrs)
    ]

instance RunMessage Vision where
  runMessage msg (Vision attrs) = Vision <$> runMessage msg attrs

newtype VisionEffect = VisionEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget)

visionEffect :: CardEffect VisionEffect
visionEffect =
  effectWith VisionEffect Cards.vision (endsL ?~ DisableAtEndOfPhase)

instance RunMessage VisionEffect where
  runMessage msg e@(VisionEffect attrs) = case msg of
    EffectMessage ident msg' | ident == effectId attrs -> case msg' of
      UsedEffect identityId -> do
        chooseOne
          identityId
          [ Label "THW" [Run [EffectMessage ident $ EffectChoice 1]]
          , Label "ATK" [Run [EffectMessage ident $ EffectChoice 2]]
          ]
        pure e
      EffectChoice n -> case n of
        1 -> pure . VisionEffect $ attrs & modifiersL .~ [ThwartModifier 2]
        2 -> pure . VisionEffect $ attrs & modifiersL .~ [AttackModifier 2]
        _ -> error "Invalid choice"
      _ -> VisionEffect <$> runMessage msg attrs
    _ -> VisionEffect <$> runMessage msg attrs
