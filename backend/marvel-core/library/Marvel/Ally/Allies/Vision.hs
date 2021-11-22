module Marvel.Ally.Allies.Vision
  ( vision
  , visionEffect
  , Vision(..)
  , VisionEffect(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Stats
import Marvel.Target

vision :: AllyCard Vision
vision = ally Vision Cards.vision (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Vision = Vision AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities Vision where
  getAbilities (Vision a) =
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
            (TargetMatches $ AllyEntity $ AllyWithId $ toId a)
    ]

instance RunMessage Vision where
  runMessage msg (Vision attrs) = Vision <$> runMessage msg attrs

newtype VisionEffect = VisionEffect EffectAttrs
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

visionEffect :: CardEffect VisionEffect
visionEffect =
  effectWith VisionEffect Cards.vision (endsL ?~ DisableAtEndOfPhase)

instance RunMessage VisionEffect where
  runMessage msg e@(VisionEffect attrs) = case msg of
    EffectMessage effectId msg' | effectId == toId attrs -> case msg' of
      UsedEffect ident -> do
        chooseOne
          ident
          [ Label "THW" [Run [EffectMessage effectId $ EffectChoice 1]]
          , Label "ATK" [Run [EffectMessage effectId $ EffectChoice 2]]
          ]
        pure e
      EffectChoice n -> case n of
        1 -> pure . VisionEffect $ attrs & modifiersL .~ [ThwartModifier 2]
        2 -> pure . VisionEffect $ attrs & modifiersL .~ [AttackModifier 2]
        _ -> error "Invalid choice"
      _ -> VisionEffect <$> runMessage msg attrs
    _ -> VisionEffect <$> runMessage msg attrs