module Marvel.Support.Supports.Helicarrier where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

helicarrier :: SupportCard Helicarrier
helicarrier = support Helicarrier Cards.helicarrier

instance HasAbilities Helicarrier where
  getAbilities a =
    [ ability a 1 Action NoCriteria ExhaustCost $
        CreateEffect Cards.helicarrier (toSource a) ChooseAPlayer
    ]

newtype Helicarrier = Helicarrier (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Helicarrier where
  runMessage msg (Helicarrier a) = Helicarrier <$> runMessage msg a

newtype HelicarrierEffect = HelicarrierEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

helicarrierEffect :: CardEffect HelicarrierEffect
helicarrierEffect =
  effectWith HelicarrierEffect Cards.helicarrier $
    (modifiersL .~ [ResourceCostReduction 1])
      . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage HelicarrierEffect where
  runMessage msg e@(HelicarrierEffect attrs) = case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      pushWhenM
        (effectValidFor attrs (toRef ident))
        (EffectMessage (effectId attrs) DisableEffect)
      pure e
    _ -> HelicarrierEffect <$> runMessage msg attrs
