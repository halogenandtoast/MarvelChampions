module Marvel.Support.Supports.Helicarrier where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Attrs
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

helicarrier :: SupportCard Helicarrier
helicarrier = support Helicarrier Cards.helicarrier

instance HasAbilities Helicarrier where
  getAbilities a =
    [ ability a 1 Action NoCriteria ExhaustCost
        $ CreateEffect Cards.helicarrier (toSource a) ChooseAPlayer
    ]

newtype Helicarrier = Helicarrier SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Helicarrier where
  runMessage msg (Helicarrier a) = Helicarrier <$> runMessage msg a

newtype HelicarrierEffect = HelicarrierEffect EffectAttrs
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

helicarrierEffect :: CardEffect HelicarrierEffect
helicarrierEffect =
  effectWith HelicarrierEffect Cards.helicarrier
    $ (modifiersL .~ [ResourceCostReduction 1])
    . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage HelicarrierEffect where
  runMessage msg e@(HelicarrierEffect attrs) = case msg of
    IdentityMessage ident (PlayedCard _ _) -> e <$ whenM
      (effectValidFor attrs (IdentityTarget ident))
      (push $ EffectMessage (toId attrs) DisableEffect)
    _ -> HelicarrierEffect <$> runMessage msg attrs
