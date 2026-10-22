module Marvel.Support.Supports.Helicarrier where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def (CardDef)
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

-- | The next card a player plays this phase costs 1 less: Helicarrier's, and the same
-- for other cards that say so.
newtype NextCardCostsLessEffect = NextCardCostsLessEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

helicarrierEffect :: CardEffect NextCardCostsLessEffect
helicarrierEffect = nextCardCostsLess Cards.helicarrier

nextCardCostsLess :: CardDef -> CardEffect NextCardCostsLessEffect
nextCardCostsLess def =
  effectWith NextCardCostsLessEffect def $
    (modifiersL .~ [ResourceCostReduction 1])
      . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage NextCardCostsLessEffect where
  runMessage msg e@(NextCardCostsLessEffect attrs) = case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      pushWhenM
        (effectValidFor attrs (toRef ident))
        (EffectMessage (effectId attrs) DisableEffect)
      pure e
    _ -> NextCardCostsLessEffect <$> runMessage msg attrs
